use std::path::PathBuf;

use once_cell::sync::Lazy;

#[cfg(feature = "clap")]
const ENV_PATH_SEP: char = if cfg!(windows) { ';' } else { ':' };

#[derive(Debug, Clone)]
#[cfg_attr(feature = "clap", derive(clap::Parser))]
#[cfg_attr(feature = "clap", clap(name = "tinymist", author, version, about, long_version(LONG_VERSION.as_str())))]
pub struct CliArguments {
    /// Mode of the binary
    #[cfg_attr(
        feature = "clap",
        clap(long, default_value = "server", value_name = "FILE")
    )]
    pub mode: String,
    /// Mirror the stdin to the file
    #[cfg_attr(feature = "clap", clap(long, default_value = "", value_name = "FILE"))]
    pub mirror: String,
    /// Replay input from the file
    #[cfg_attr(feature = "clap", clap(long, default_value = "", value_name = "FILE"))]
    pub replay: String,
    /// Font paths, which doesn't allow for dynamic configuration
    #[cfg_attr(feature = "clap", clap(
        long = "font-path",
        value_name = "DIR",
        action = clap::ArgAction::Append,
        env = "TYPST_FONT_PATHS",
        value_delimiter = ENV_PATH_SEP
    ))]
    pub font_paths: Vec<PathBuf>,
    /// Exclude system fonts
    #[cfg_attr(feature = "clap", clap(long, default_value = "false"))]
    pub no_system_fonts: bool,
}

pub static LONG_VERSION: Lazy<String> = Lazy::new(|| {
    format!(
        "
Build Timestamp:     {}
Build Git Describe:  {}
Commit SHA:          {}
Commit Date:         {}
Commit Branch:       {}
Cargo Target Triple: {}
Typst Version:       {}
",
        env!("VERGEN_BUILD_TIMESTAMP"),
        env!("VERGEN_GIT_DESCRIBE"),
        option_env!("VERGEN_GIT_SHA").unwrap_or("None"),
        option_env!("VERGEN_GIT_COMMIT_TIMESTAMP").unwrap_or("None"),
        option_env!("VERGEN_GIT_BRANCH").unwrap_or("None"),
        env!("VERGEN_CARGO_TARGET_TRIPLE"),
        env!("TYPST_VERSION"),
    )
});