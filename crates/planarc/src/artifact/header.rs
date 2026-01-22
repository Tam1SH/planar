pub const MAGIC: &[u8; 4] = b"PDLA";
pub const VERSION: u32 = 1;

const RAW_FINGERPRINT: &str = env!("PLANAR_COMPILER_FINGERPRINT");
pub const COMPILER_BUILDID: u64 = parse_u64_const(RAW_FINGERPRINT);

const fn parse_u64_const(s: &str) -> u64 {
    let b = s.as_bytes();
    let mut res = 0u64;
    let mut i = 0;
    while i < b.len() {
        res = res * 10 + (b[i] - b'0') as u64;
        i += 1;
    }
    res
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct Header {
    pub magic: [u8; 4],
    pub version: u32,
    pub build_id: u64,
    pub checksum: u64,
}

impl Header {
    pub fn new(payload_checksum: u64, build_id: Option<u64>) -> Self {
        Self {
            magic: *MAGIC,
            version: VERSION,
            build_id: build_id.unwrap_or(COMPILER_BUILDID),
            checksum: payload_checksum,
        }
    }

    pub fn as_bytes(&self) -> [u8; 24] {
        unsafe { std::mem::transmute(*self) }
    }

    pub fn from_bytes(bytes: &[u8; 24]) -> Self {
        unsafe { std::mem::transmute(*bytes) }
    }
}
