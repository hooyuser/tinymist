use std::path::{Path, PathBuf};
use std::sync::{Arc, mpsc};
use std::thread;
use std::time::Duration;

use comemo::Track;
use dapts::{CompletionItem, ProcessEventStartMethod, StoppedEventReason, ThreadEventReason};
use reflexo::ImmutPath;
use reflexo_typst::{EntryReader, TaskInputs};
use serde::Deserialize;
use sync_ls::{internal_error, invalid_params, invalid_request, just_ok, just_result, SchedulableResponse};
use tinymist_std::error::prelude::*;
use typst::{
    foundations::Repr,
    routines::EvalMode,
    syntax::{Source, Span},
    World,
};

use super::*;

impl ServerState {
    /// Called at the end of the configuration sequence.
    /// Indicates that all breakpoints etc. have been sent to the DA and that
    /// the 'launch' can start.
    pub(crate) fn configuration_done(
        &mut self,
        _args: dapts::ConfigurationDoneArguments,
    ) -> SchedulableResponse<()> {
        just_ok(())
    }

    /// Should stop the debug session.
    pub(crate) fn disconnect(
        &mut self,
        _args: dapts::DisconnectArguments,
    ) -> SchedulableResponse<()> {
        let _ = self.debug.session.take();

        just_ok(())
    }

    pub(crate) fn terminate_debug(
        &mut self,
        _args: dapts::TerminateArguments,
    ) -> SchedulableResponse<()> {
        let _ = self.debug.session.take();

        self.client
            .send_dap_event::<dapts::event::Terminated>(dapts::TerminatedEvent { restart: None });

        just_ok(())
    }

    pub(crate) fn terminate_debug_thread(
        &mut self,
        args: dapts::TerminateThreadsArguments,
    ) -> SchedulableResponse<()> {
        if args.thread_ids.as_ref().is_none_or(|id| id.is_empty()) {
            return just_ok(());
        }
        let terminate_thread_ok = args.thread_ids.into_iter().flatten().all(|id| id == 1);
        if terminate_thread_ok {
            let _ = self.debug.session.take();
        }

        just_ok(())
    }

    // cancelRequest

    pub(crate) fn attach_debug(
        &mut self,
        args: dapts::AttachRequestArguments,
    ) -> SchedulableResponse<()> {
        self.launch_debug_(
            dapts::LaunchRequestArguments { raw: args.raw },
            ProcessEventStartMethod::Attach,
        )
    }

    pub(crate) fn launch_debug(
        &mut self,
        args: dapts::LaunchRequestArguments,
    ) -> SchedulableResponse<()> {
        self.launch_debug_(args, ProcessEventStartMethod::Launch)
    }

    pub(crate) fn launch_debug_(
        &mut self,
        args: dapts::LaunchRequestArguments,
        method: ProcessEventStartMethod,
    ) -> SchedulableResponse<()> {
        // wait 1 second until configuration has finished (and configurationDoneRequest
        // has been called) await this._configurationDone.wait(1000);

        // start the program in the runtime
        let args = serde_json::from_value::<LaunchDebugArguments>(args.raw).unwrap();

        let program: ImmutPath = Path::new(&args.program).into();
        let root = Path::new(&args.root).into();
        let input = self.resolve_task(program.clone());
        let entry = self
            .entry_resolver()
            .resolve_with_root(Some(root), Some(program));

        // todo: respect lock file
        let input = TaskInputs {
            entry: Some(entry),
            inputs: input.inputs,
        };

        let snapshot = self.project.snapshot().unwrap().snap.clone().task(input);
        let world = &snapshot.world;

        let thread_id = 1u64;
        let shared = Arc::new(DebugRuntimeShared::new());
        let (tx, rx) = mpsc::channel();

        let adaptor = Arc::new(ServerDebugAdaptor::new(
            self.client.clone(),
            shared.clone(),
            thread_id,
        ));

        tinymist_dap::start_session(world.clone(), adaptor, rx);

        let stop_on_entry = args.stop_on_entry.unwrap_or_default();

        self.debug.session = Some(DebugSession {
            config: self.config.const_dap_config.clone(),
            snapshot,
            stop_on_entry,
            thread_id,
            runtime_tx: tx,
            shared,
        });

        if stop_on_entry {
            for _ in 0..10 {
                if with_debug_session_mut(|session| {
                    session.request_auto_break();
                })
                .is_some()
                {
                    break;
                }
                thread::sleep(Duration::from_millis(10));
            }
        }

        self.client
            .send_dap_event::<dapts::event::Process>(dapts::ProcessEvent {
                name: "typst".into(),
                start_method: Some(method),
                ..dapts::ProcessEvent::default()
            });

        self.client
            .send_dap_event::<dapts::event::Thread>(dapts::ThreadEvent {
                reason: ThreadEventReason::Started,
                thread_id: self.debug.session()?.thread_id,
            });

        if let Err(err) = self.resume_debugger(ResumeMode::Continue) {
            log::error!("failed to start debugger: {err:?}");
        }

        just_ok(())
    }

    // customRequest
}

impl ServerState {
    fn resume_debugger(&mut self, mode: ResumeMode) -> LspResult<()> {
        let session = self.debug.session()?;

        if with_debug_session_mut(|dbg| dbg.plan_resume(mode)).is_none() {
            return Err(invalid_request("Debug session not initialized"));
        }

        {
            let mut status = session.shared.status.lock();
            status.stopped = false;
            status.reason = None;
            status.breakpoint = None;
        }

        session
            .runtime_tx
            .send(DebugRequest::Continue)
            .map_err(|e| internal_error(format!("debug runtime unavailable: {e}")))?;

        self.client
            .send_dap_event::<dapts::event::Continued>(dapts::ContinuedEvent {
                thread_id: session.thread_id,
                all_threads_continued: Some(true),
            });

        Ok(())
    }

    pub(crate) fn continue_debug(
        &mut self,
        args: dapts::ContinueArguments,
    ) -> SchedulableResponse<dapts::ContinueResponse> {
        let thread_id = self.debug.session()?.thread_id;
        if args.thread_id != thread_id {
            return just_result(Err(invalid_request("Unknown thread id")));
        }

        just_result(self.resume_debugger(ResumeMode::Continue).map(|_| {
            dapts::ContinueResponse {
                all_threads_continued: Some(true),
            }
        }))
    }

    pub(crate) fn next_debug(
        &mut self,
        args: dapts::NextArguments,
    ) -> SchedulableResponse<()> {
        let thread_id = self.debug.session()?.thread_id;
        if args.thread_id != thread_id {
            return just_result(Err(invalid_request("Unknown thread id")));
        }

        just_result(self.resume_debugger(ResumeMode::StepOver))
    }

    pub(crate) fn step_in_debug(
        &mut self,
        args: dapts::StepInArguments,
    ) -> SchedulableResponse<()> {
        let thread_id = self.debug.session()?.thread_id;
        if args.thread_id != thread_id {
            return just_result(Err(invalid_request("Unknown thread id")));
        }

        just_result(self.resume_debugger(ResumeMode::StepIn))
    }

    pub(crate) fn step_out_debug(
        &mut self,
        args: dapts::StepOutArguments,
    ) -> SchedulableResponse<()> {
        let thread_id = self.debug.session()?.thread_id;
        if args.thread_id != thread_id {
            return just_result(Err(invalid_request("Unknown thread id")));
        }

        just_result(self.resume_debugger(ResumeMode::StepOut))
    }

    pub(crate) fn stack_trace(
        &mut self,
        args: dapts::StackTraceArguments,
    ) -> SchedulableResponse<dapts::StackTraceResponse> {
        let session = self.debug.session()?;
        if args.thread_id != session.thread_id {
            return just_result(Err(invalid_request("Unknown thread id")));
        }

        let breakpoint = {
            let status = session.shared.status.lock();
            status.breakpoint.ok_or_else(|| invalid_request("Debugger is not paused"))?
        };

        let world = &session.snapshot.world;
        let source = world.source(breakpoint.file_id).map_err(invalid_request)?;

        let info_arc = with_debug_session(|dbg| {
            dbg.breakpoint_info(breakpoint.file_id).cloned()
        })
        .flatten()
        .ok_or_else(|| invalid_request("Missing breakpoint metadata"))?;

        let mut chain = Vec::new();
        let mut current = breakpoint.index;
        loop {
            chain.push(current);
            let Some(item) = info_arc.meta.get(current) else {
                break;
            };
            if let Some(parent) = item.parent {
                current = parent;
            } else {
                break;
            }
        }

        let info = &info_arc;

        let dap_source = session.to_dap_source(breakpoint.file_id);
        let mut stack_frames = Vec::new();

        for (depth, idx) in chain.into_iter().enumerate() {
            let (kind, span) = if depth == 0 {
                (breakpoint.kind, breakpoint.span)
            } else if let Some(item) = info.meta.get(idx) {
                (item.kind, item.origin_span)
            } else {
                (breakpoint.kind, breakpoint.span)
            };

            let (start_pos, end_pos, has_range) = Self::span_positions(&session, &source, span);
            stack_frames.push(dapts::StackFrame {
                id: Self::frame_id(breakpoint.file_id, idx),
                name: Self::frame_name(kind),
                line: Self::to_u32(start_pos.line),
                column: Self::to_u32(start_pos.character),
                end_line: has_range.then(|| Self::to_u32(end_pos.line)),
                end_column: has_range.then(|| Self::to_u32(end_pos.character)),
                source: Some(dap_source.clone()),
                can_restart: None,
                instruction_pointer_reference: None,
                module_id: None,
                presentation_hint: None,
            });
        }

        just_ok(dapts::StackTraceResponse {
            stack_frames,
            total_frames: None,
        })
    }

    pub(crate) fn scopes_debug(
        &mut self,
        _args: dapts::ScopesArguments,
    ) -> SchedulableResponse<dapts::ScopesResponse> {
        just_ok(dapts::ScopesResponse { scopes: Vec::new() })
    }

    pub(crate) fn variables_debug(
        &mut self,
        _args: dapts::VariablesArguments,
    ) -> SchedulableResponse<dapts::VariablesResponse> {
        just_ok(dapts::VariablesResponse {
            variables: Vec::new(),
        })
    }

    fn frame_id(file_id: FileId, index: usize) -> u64 {
        let raw = file_id.into_raw().get() as u64;
        (raw << 32) | index as u64
    }

    fn frame_name(kind: BreakpointKind) -> String {
        format!("{kind:?}")
    }

    fn to_u32(value: u64) -> u32 {
        value.min(u32::MAX as u64) as u32
    }

    fn span_positions(
        session: &DebugSession,
        source: &Source,
        span: Span,
    ) -> (DapPosition, DapPosition, bool) {
        if let Some(range) = source.range(span) {
            let start = session.to_dap_position(range.start, source);
            let end = session.to_dap_position(range.end, source);
            let has_range = range.end > range.start;
            (start, end, has_range)
        } else {
            let pos = session.to_dap_position(0, source);
            (pos, pos, false)
        }
    }
}

/// This interface describes the mock-debug specific launch attributes
/// (which are not part of the Debug Adapter Protocol).
/// The schema for these attributes lives in the package.json of the mock-debug
/// extension. The interface should always match this schema.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
struct LaunchDebugArguments {
    /// An absolute path to the "program" to debug.
    program: String,
    /// The root directory of the program (used to resolve absolute paths).
    root: String,
    /// Automatically stop target after launch. If not specified, target does
    /// not stop.
    stop_on_entry: Option<bool>,
}

impl ServerState {
    pub(crate) fn debug_threads(
        &mut self,
        _args: (),
    ) -> SchedulableResponse<dapts::ThreadsResponse> {
        just_ok(dapts::ThreadsResponse {
            threads: vec![dapts::Thread {
                id: 1,
                name: "thread 1".into(),
            }],
        })
    }
}

impl ServerState {
    pub(crate) fn evaluate_repl(
        &mut self,
        args: dapts::EvaluateArguments,
    ) -> SchedulableResponse<dapts::EvaluateResponse> {
        let session = self.debug.session()?;
        let world = &session.snapshot.world;
        let library = &world.library;

        let (source, span) = {
            let status = session.shared.status.lock();
            if let Some(bp) = status.breakpoint {
                let source = world.source(bp.file_id).map_err(invalid_request)?;
                (source, bp.span)
            } else {
                let main = world
                    .main_id()
                    .ok_or_else(|| invalid_request("No main file found"))?;
                let source = world.source(main).map_err(invalid_request)?;
                (source, Span::detached())
            }
        };

        let effective_span = source
            .find(span)
            .map(|node| node.span())
            .unwrap_or(span);

        let scope_source = typst_shim::eval::eval_compat(&world, &source)
            .map_err(|e| invalid_params(format!("{e:?}")))?;

        let val = typst_shim::eval::eval_string(
            &typst::ROUTINES,
            (world as &dyn World).track(),
            &args.expression,
            effective_span,
            EvalMode::Code,
            scope_source.scope().clone(),
        )
        .map_err(|e| invalid_params(format!("{e:?}")))?;

        just_ok(dapts::EvaluateResponse {
            result: format!("{}", val.repr()),
            ty: Some(format!("{}", val.ty().repr())),
            ..dapts::EvaluateResponse::default()
        })
    }

    pub(crate) fn complete_repl(
        &mut self,
        args: dapts::CompletionsArguments,
    ) -> SchedulableResponse<dapts::CompletionsResponse> {
        let _ = args;
        let session = self
            .debug
            .session
            .as_ref()
            .ok_or_else(|| internal_error("No debug session found"))?;

        just_ok(dapts::CompletionsResponse { targets: vec![] })
    }
}
