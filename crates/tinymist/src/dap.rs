#![allow(unused)]

mod event;
mod init;
mod request;

pub use init::*;

use parking_lot::Mutex;
use dapts::StoppedEventReason;
use reflexo_typst::vfs::PathResolution;
use std::sync::{Arc, mpsc};
use serde::{Deserialize, Serialize};
use sync_ls::{internal_error, invalid_request, LspResult, TypedLspClient};
use tinymist_query::PositionEncoding;
use tinymist_debug::{
    ActiveBreakpoint, BreakpointKind, ResumeMode, with_debug_session, with_debug_session_mut,
};
use tinymist_dap::{self, DebugAdaptor, DebugRequest};
// use sync_lsp::RequestId;
use typst::{
    diag::{SourceResult, Warned},
    foundations::Value,
    layout::PagedDocument,
    syntax::{FileId, Source},
};

use crate::project::LspCompileSnapshot;
use crate::{ConstDapConfig, ServerState};

#[derive(Default)]
pub(crate) struct DebugState {
    pub(crate) session: Option<DebugSession>,
}

impl DebugState {
    pub(crate) fn session(&self) -> LspResult<&DebugSession> {
        self.session
            .as_ref()
            .ok_or_else(|| invalid_request("No active debug session"))
    }
}

#[derive(Default)]
pub(crate) struct RuntimeStatus {
    pub stopped: bool,
    pub reason: Option<ResumeMode>,
    pub breakpoint: Option<ActiveBreakpoint>,
}

pub(crate) struct DebugRuntimeShared {
    pub status: Mutex<RuntimeStatus>,
}

impl DebugRuntimeShared {
    pub fn new() -> Self {
        Self {
            status: Mutex::new(RuntimeStatus::default()),
        }
    }
}

pub(crate) struct DebugSession {
    config: ConstDapConfig,

    snapshot: LspCompileSnapshot,
    /// A faked thread id. We don't support multiple threads, so we can use a
    /// hardcoded ID for the default thread.
    thread_id: u64,
    /// Whether the debugger should stop on entry.
    stop_on_entry: bool,

    runtime_tx: mpsc::Sender<DebugRequest>,
    shared: Arc<DebugRuntimeShared>,
}
// private _variableHandles = new Handles<"locals" | "globals" |
// RuntimeVariable>();

//     private _valuesInHex = false;
//     private _useInvalidatedEvent = false;

const DAP_POS_ENCODING: PositionEncoding = PositionEncoding::Utf16;

impl DebugSession {
    pub fn to_dap_source(&self, id: FileId) -> dapts::Source {
        use dapts::Source;
        Source {
            path: match self.snapshot.world.path_for_id(id).ok() {
                Some(PathResolution::Resolved(path)) => Some(path.display().to_string()),
                None | Some(PathResolution::Rootless(..)) => None,
            },
            ..Source::default()
        }
    }

    pub fn to_dap_position(&self, pos: usize, source: &Source) -> DapPosition {
        let mut lsp_pos = tinymist_query::to_lsp_position(pos, DAP_POS_ENCODING, source);

        if self.config.lines_start_at1 {
            lsp_pos.line += 1;
        }
        if self.config.columns_start_at1 {
            lsp_pos.character += 1;
        }

        DapPosition {
            line: lsp_pos.line as u64,
            character: lsp_pos.character as u64,
        }
    }
}

/// Position in a text document expressed as line and character offset.
/// A position is between two characters like an 'insert' cursor in a editor.
///
/// Whether or not the line and column are 0 or 1-based is negotiated between
/// the client and server.
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Default, Deserialize, Serialize)]
pub struct DapPosition {
    /// Line position in a document.
    pub line: u64,
    /// Character offset on a line in a document.
    ///
    /// If the character value is greater than the line length it defaults back
    /// to the line length.
    pub character: u64,
}

struct ServerDebugAdaptor {
    client: TypedLspClient<ServerState>,
    shared: Arc<DebugRuntimeShared>,
    thread_id: u64,
}

impl ServerDebugAdaptor {
    fn new(
        client: TypedLspClient<ServerState>,
        shared: Arc<DebugRuntimeShared>,
        thread_id: u64,
    ) -> Self {
        Self {
            client,
            shared,
            thread_id,
        }
    }

    fn map_reason(reason: ResumeMode) -> StoppedEventReason {
        match reason {
            ResumeMode::StepIn | ResumeMode::StepOut | ResumeMode::StepOver => {
                StoppedEventReason::Step
            }
            ResumeMode::Continue => StoppedEventReason::Breakpoint,
            ResumeMode::Pause => StoppedEventReason::Entry,
        }
    }
}

impl DebugAdaptor for ServerDebugAdaptor {
    fn before_compile(&self) {}

    fn after_compile(&self, _result: Warned<SourceResult<PagedDocument>>) {}

    fn terminate(&self) {
        self.client
            .send_dap_event::<dapts::event::Terminated>(dapts::TerminatedEvent {
                restart: None,
            });
    }

    fn stopped(&self, _ctx: &tinymist_dap::BreakpointContext) {
        let (reason, breakpoint) = with_debug_session_mut(|session| {
            let reason = session.take_stop_reason().unwrap_or(ResumeMode::Pause);
            let bp = session.current();
            (reason, bp)
        })
        .unwrap_or((ResumeMode::Pause, None));

        {
            let mut status = self.shared.status.lock();
            status.stopped = true;
            status.reason = Some(reason);
            status.breakpoint = breakpoint;
        }

        self.client
            .send_dap_event::<dapts::event::Stopped>(dapts::StoppedEvent {
                all_threads_stopped: Some(true),
                reason: Self::map_reason(reason),
                description: None,
                thread_id: Some(self.thread_id),
                hit_breakpoint_ids: None,
                preserve_focus_hint: Some(false),
                text: None,
            });
    }

    fn respond(&self, _id: i64, _result: SourceResult<Value>) {
        // TODO: hook REPL evaluate output through DAP responses.
    }
}
