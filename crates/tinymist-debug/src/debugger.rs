//! Tinymist breakpoint support for Typst.

mod instr;

use std::sync::Arc;

use comemo::Tracked;
use parking_lot::RwLock;
use tinymist_std::hash::{FxHashMap, FxHashSet};
use tinymist_world::vfs::FileId;
use typst::World;
use typst::diag::FileResult;
use typst::engine::Engine;
use typst::foundations::{Binding, Context, Dict, Scopes, func};
use typst::syntax::{Source, Span};

use crate::instrument::Instrumenter;

#[derive(Default)]
pub struct BreakpointInstr {}

/// The kind of breakpoint.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BreakpointKind {
    // Expr,
    // Line,
    /// A call breakpoint.
    CallStart,
    /// A call breakpoint.
    CallEnd,
    /// A function breakpoint.
    Function,
    /// A break breakpoint.
    Break,
    /// A continue breakpoint.
    Continue,
    /// A return breakpoint.
    Return,
    /// A block start breakpoint.
    BlockStart,
    /// A block end breakpoint.
    BlockEnd,
    /// A show start breakpoint.
    ShowStart,
    /// A show end breakpoint.
    ShowEnd,
    /// A doc start breakpoint.
    DocStart,
    /// A doc end breakpoint.
    DocEnd,
    /// A before compile breakpoint.
    BeforeCompile,
    /// A after compile breakpoint.
    AfterCompile,
}

impl BreakpointKind {
    /// Converts the breakpoint kind to a string.
    pub fn to_str(self) -> &'static str {
        match self {
            BreakpointKind::CallStart => "call_start",
            BreakpointKind::CallEnd => "call_end",
            BreakpointKind::Function => "function",
            BreakpointKind::Break => "break",
            BreakpointKind::Continue => "continue",
            BreakpointKind::Return => "return",
            BreakpointKind::BlockStart => "block_start",
            BreakpointKind::BlockEnd => "block_end",
            BreakpointKind::ShowStart => "show_start",
            BreakpointKind::ShowEnd => "show_end",
            BreakpointKind::DocStart => "doc_start",
            BreakpointKind::DocEnd => "doc_end",
            BreakpointKind::BeforeCompile => "before_compile",
            BreakpointKind::AfterCompile => "after_compile",
        }
    }
}

/// The mode used to resume execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResumeMode {
    /// Stay paused.
    Pause,
    /// Continue without pausing unless another breakpoint is hit.
    Continue,
    /// Step into the next executable region.
    StepIn,
    /// Step out of the current region.
    StepOut,
    /// Step over the current region.
    StepOver,
}

impl Default for ResumeMode {
    fn default() -> Self {
        ResumeMode::Pause
    }
}

/// Metadata describing an active breakpoint hit.
#[derive(Debug, Clone, Copy)]
pub struct ActiveBreakpoint {
    /// Source file containing the breakpoint.
    pub file_id: FileId,
    /// Index into [`BreakpointInfo::meta`] for this breakpoint.
    pub index: usize,
    /// The kind of instrumentation that triggered the pause.
    pub kind: BreakpointKind,
    /// Span that should be highlighted for the current stop.
    pub span: Span,
    /// Parent block start index, if any.
    pub parent: Option<usize>,
    /// Matching block end/start index for paired breakpoints.
    pub counterpart: Option<usize>,
}

#[derive(Default)]
pub struct BreakpointInfo {
    pub meta: Vec<BreakpointItem>,
}

pub struct BreakpointItem {
    pub origin_span: Span,
    pub kind: BreakpointKind,
    pub parent: Option<usize>,
    pub counterpart: Option<usize>,
}

static DEBUG_SESSION: RwLock<Option<DebugSession>> = RwLock::new(None);

/// The debug session handler.
pub trait DebugSessionHandler: Send + Sync {
    /// Called when a breakpoint is hit.
    fn on_breakpoint(
        &self,
        engine: &Engine,
        context: Tracked<Context>,
        scopes: Scopes,
        span: Span,
        kind: BreakpointKind,
    );
}

/// The debug session.
pub struct DebugSession {
    enabled: FxHashSet<(FileId, usize, BreakpointKind)>,
    /// The breakpoint meta.
    breakpoints: FxHashMap<FileId, Arc<BreakpointInfo>>,

    /// The handler.
    pub handler: Arc<dyn DebugSessionHandler>,

    current: Option<ActiveBreakpoint>,
    last_resume: ResumeMode,
    stop_reason: Option<ResumeMode>,
    auto_break: bool,
}

impl DebugSession {
    /// Creates a new debug session.
    pub fn new(handler: Arc<dyn DebugSessionHandler>) -> Self {
        Self {
            enabled: FxHashSet::default(),
            breakpoints: FxHashMap::default(),
            handler,
            current: None,
            last_resume: ResumeMode::Pause,
            stop_reason: None,
            auto_break: false,
        }
    }

    /// Returns the currently active breakpoint, if any.
    pub fn current(&self) -> Option<ActiveBreakpoint> {
        self.current
    }

    /// Returns the reason of the most recent stop.
    pub fn stop_reason(&self) -> Option<ResumeMode> {
        self.stop_reason
    }

    /// Returns the metadata for a file.
    pub fn breakpoint_info(&self, file_id: FileId) -> Option<&Arc<BreakpointInfo>> {
        self.breakpoints.get(&file_id)
    }

    /// Consumes the current stop reason.
    pub fn take_stop_reason(&mut self) -> Option<ResumeMode> {
        self.stop_reason.take()
    }

    /// Configures the next resume action.
    pub fn plan_resume(&mut self, mode: ResumeMode) -> bool {
        self.last_resume = mode;
        self.stop_reason = None;

        match mode {
            ResumeMode::Pause => {
                self.enabled.clear();
                false
            }
            ResumeMode::Continue => {
                self.enabled.clear();
                self.current = None;
                true
            }
            ResumeMode::StepIn | ResumeMode::StepOut | ResumeMode::StepOver => {
                let Some(active) = self.current else {
                    self.enabled.clear();
                    return false;
                };
                let Some((target_idx, kind)) = self.compute_step_target(&active, mode) else {
                    self.enabled.clear();
                    self.current = None;
                    return false;
                };
                self.enabled.clear();
                self.enabled.insert((active.file_id, target_idx, kind));
                self.current = None;
                true
            }
        }
    }

    /// Requests the next breakpoint to trigger regardless of enabled set.
    pub fn request_auto_break(&mut self) {
        self.auto_break = true;
    }

    fn compute_step_target(
        &self,
        active: &ActiveBreakpoint,
        mode: ResumeMode,
    ) -> Option<(usize, BreakpointKind)> {
        let info = self.breakpoints.get(&active.file_id)?;
        let target_idx = match mode {
            ResumeMode::StepIn => Self::target_step_in(info, active),
            ResumeMode::StepOut => Self::target_step_out(info, active),
            ResumeMode::StepOver => Self::target_step_over(info, active),
            ResumeMode::Pause | ResumeMode::Continue => None,
        }?;
        let kind = info.meta.get(target_idx)?.kind;
        Some((target_idx, kind))
    }

    fn target_step_in(info: &BreakpointInfo, active: &ActiveBreakpoint) -> Option<usize> {
        Self::find_first_child(info, active.index)
            .or_else(|| Self::target_step_over(info, active))
    }

    fn target_step_out(info: &BreakpointInfo, active: &ActiveBreakpoint) -> Option<usize> {
        if active.kind == BreakpointKind::BlockStart {
            if let Some(idx) = info.meta.get(active.index)?.counterpart {
                return Some(idx);
            }
        }
        if let Some(parent_start) = active.parent {
            let parent_item = info.meta.get(parent_start)?;
            parent_item.counterpart
        } else {
            None
        }
    }

    fn target_step_over(info: &BreakpointInfo, active: &ActiveBreakpoint) -> Option<usize> {
        if active.kind == BreakpointKind::BlockStart {
            if let Some(idx) = info.meta.get(active.index)?.counterpart {
                return Some(idx);
            }
        }
        Self::find_next_in_scope(info, active.index, active.parent)
    }

    fn find_first_child(info: &BreakpointInfo, start_idx: usize) -> Option<usize> {
        info.meta
            .iter()
            .enumerate()
            .skip(start_idx + 1)
            .find(|(_, item)| item.parent == Some(start_idx))
            .map(|(idx, _)| idx)
    }

    fn find_next_in_scope(
        info: &BreakpointInfo,
        start_idx: usize,
        parent: Option<usize>,
    ) -> Option<usize> {
        info.meta
            .iter()
            .enumerate()
            .skip(start_idx + 1)
            .find(|(_, item)| item.parent == parent)
            .map(|(idx, _)| idx)
    }

    fn should_break(&self, fid: FileId, id: usize, kind: BreakpointKind) -> bool {
        self.auto_break || self.enabled.contains(&(fid, id, kind))
    }

    fn on_breakpoint_hit(
        &mut self,
        fid: FileId,
        id: usize,
        kind: BreakpointKind,
    ) -> Option<ActiveBreakpoint> {
        let info = self.breakpoints.get(&fid)?;
        let item = info.meta.get(id)?;
        let active = ActiveBreakpoint {
            file_id: fid,
            index: id,
            kind,
            span: item.origin_span,
            parent: item.parent,
            counterpart: item.counterpart,
        };

        self.current = Some(active);
        self.stop_reason = Some(self.last_resume);
        self.last_resume = ResumeMode::Pause;
        self.enabled.remove(&(fid, id, kind));
        self.auto_break = false;

        Some(active)
    }
}

/// Runs function with the debug session.
pub fn with_debug_session<F, R>(f: F) -> Option<R>
where
    F: FnOnce(&DebugSession) -> R,
{
    Some(f(DEBUG_SESSION.read().as_ref()?))
}

/// Runs function with mutable access to the debug session.
pub fn with_debug_session_mut<F, R>(f: F) -> Option<R>
where
    F: FnOnce(&mut DebugSession) -> R,
{
    Some(f(DEBUG_SESSION.write().as_mut()?))
}

/// Sets the debug session.
pub fn set_debug_session(session: Option<DebugSession>) -> bool {
    let mut lock = DEBUG_SESSION.write();

    if session.is_some() && lock.is_some() {
        return false;
    }

    let _ = std::mem::replace(&mut *lock, session);
    true
}

/// Software breakpoints
fn check_soft_breakpoint(span: Span, id: usize, kind: BreakpointKind) -> Option<bool> {
    let fid = span.id()?;

    let session = DEBUG_SESSION.read();
    let session = session.as_ref()?;

    Some(session.should_break(fid, id, kind))
}

/// Software breakpoints
fn soft_breakpoint_handle(
    engine: &Engine,
    context: Tracked<Context>,
    span: Span,
    id: usize,
    kind: BreakpointKind,
    scope: Option<Dict>,
) -> Option<()> {
    let fid = span.id()?;

    let (handler, active) = {
        let mut session = DEBUG_SESSION.write();
        let session = session.as_mut()?;

        if !session.should_break(fid, id, kind) {
            return None;
        }

        let active = session.on_breakpoint_hit(fid, id, kind)?;
        (session.handler.clone(), active)
    };

    let mut scopes = Scopes::new(Some(engine.world.library()));
    if let Some(scope) = scope {
        for (key, value) in scope.into_iter() {
            scopes.top.bind(key.into(), Binding::detached(value));
        }
    }

    handler.on_breakpoint(engine, context, scopes, active.span, active.kind);
    Some(())
}

pub mod breakpoints {

    use super::*;

    macro_rules! bp_handler {
        ($name:ident, $name2:expr, $name3:ident, $name4:expr, $title:expr, $kind:ident) => {
            #[func(name = $name2, title = $title)]
            pub fn $name(span: Span, id: usize) -> bool {
                check_soft_breakpoint(span, id, BreakpointKind::$kind).unwrap_or_default()
            }
            #[func(name = $name4, title = $title)]
            pub fn $name3(
                engine: &Engine,
                context: Tracked<Context>,
                span: Span,
                id: usize,
                scope: Option<Dict>,
            ) {
                soft_breakpoint_handle(engine, context, span, id, BreakpointKind::$kind, scope);
            }
        };
    }

    bp_handler!(
        __breakpoint_call_start,
        "__breakpoint_call_start",
        __breakpoint_call_start_handle,
        "__breakpoint_call_start_handle",
        "A Software Breakpoint at the start of a call.",
        CallStart
    );
    bp_handler!(
        __breakpoint_call_end,
        "__breakpoint_call_end",
        __breakpoint_call_end_handle,
        "__breakpoint_call_end_handle",
        "A Software Breakpoint at the end of a call.",
        CallEnd
    );
    bp_handler!(
        __breakpoint_function,
        "__breakpoint_function",
        __breakpoint_function_handle,
        "__breakpoint_function_handle",
        "A Software Breakpoint at the start of a function.",
        Function
    );
    bp_handler!(
        __breakpoint_break,
        "__breakpoint_break",
        __breakpoint_break_handle,
        "__breakpoint_break_handle",
        "A Software Breakpoint at a break.",
        Break
    );
    bp_handler!(
        __breakpoint_continue,
        "__breakpoint_continue",
        __breakpoint_continue_handle,
        "__breakpoint_continue_handle",
        "A Software Breakpoint at a continue.",
        Continue
    );
    bp_handler!(
        __breakpoint_return,
        "__breakpoint_return",
        __breakpoint_return_handle,
        "__breakpoint_return_handle",
        "A Software Breakpoint at a return.",
        Return
    );
    bp_handler!(
        __breakpoint_block_start,
        "__breakpoint_block_start",
        __breakpoint_block_start_handle,
        "__breakpoint_block_start_handle",
        "A Software Breakpoint at the start of a block.",
        BlockStart
    );
    bp_handler!(
        __breakpoint_block_end,
        "__breakpoint_block_end",
        __breakpoint_block_end_handle,
        "__breakpoint_block_end_handle",
        "A Software Breakpoint at the end of a block.",
        BlockEnd
    );
    bp_handler!(
        __breakpoint_show_start,
        "__breakpoint_show_start",
        __breakpoint_show_start_handle,
        "__breakpoint_show_start_handle",
        "A Software Breakpoint at the start of a show.",
        ShowStart
    );
    bp_handler!(
        __breakpoint_show_end,
        "__breakpoint_show_end",
        __breakpoint_show_end_handle,
        "__breakpoint_show_end_handle",
        "A Software Breakpoint at the end of a show.",
        ShowEnd
    );
    bp_handler!(
        __breakpoint_doc_start,
        "__breakpoint_doc_start",
        __breakpoint_doc_start_handle,
        "__breakpoint_doc_start_handle",
        "A Software Breakpoint at the start of a doc.",
        DocStart
    );
    bp_handler!(
        __breakpoint_doc_end,
        "__breakpoint_doc_end",
        __breakpoint_doc_end_handle,
        "__breakpoint_doc_end_handle",
        "A Software Breakpoint at the end of a doc.",
        DocEnd
    );
    bp_handler!(
        __breakpoint_before_compile,
        "__breakpoint_before_compile",
        __breakpoint_before_compile_handle,
        "__breakpoint_before_compile_handle",
        "A Software Breakpoint before compilation.",
        BeforeCompile
    );
    bp_handler!(
        __breakpoint_after_compile,
        "__breakpoint_after_compile",
        __breakpoint_after_compile_handle,
        "__breakpoint_after_compile_handle",
        "A Software Breakpoint after compilation.",
        AfterCompile
    );
}
