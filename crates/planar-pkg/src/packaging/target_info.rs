pub struct TargetInfo;

impl TargetInfo {
    pub fn os() -> &'static str {
        std::env::consts::OS
    }

    pub fn arch() -> &'static str {
        match std::env::consts::ARCH {
            "x86_64" => "X64",
            "aarch64" => "ARM64",
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

    pub fn format_grammar_name(lang: &str) -> String {
        format!("{}-{}-{}.{}", lang, Self::os(), Self::arch(), Self::ext())
    }
}
