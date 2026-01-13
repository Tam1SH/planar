use std::path::PathBuf;

pub fn os() -> &'static str {
    std::env::consts::OS
}

pub fn arch() -> &'static str {
    match std::env::consts::ARCH {
        "x86_64" => "X64",
        "aarch64" => "arm64",
        other => other,
    }
}

pub fn ext() -> &'static str {
    match std::env::consts::OS {
        "linux" => "so",
        "macos" => "dylib",
        "windows" => "dll",
        _ => "so",
    }
}

pub fn format_filename(lang: &str) -> String {
    format!("{}-{}-{}.{}", lang, os(), arch(), ext())
}

pub fn local_dir() -> PathBuf {

    if let Ok(overridden) = std::env::var("PLANAR_GRAMMARS_PATH") {
        return std::path::PathBuf::from(overridden);
    }

    dirs::data_local_dir()
        .expect("Failed to find data directory")
        .join("planar/grammars")
}

pub fn is_match(filename: &str) -> bool {
    filename.contains(os()) && filename.contains(arch())
}