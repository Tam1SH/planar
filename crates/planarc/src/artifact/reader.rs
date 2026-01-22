use rkyv::Archived;
use std::io;
use thiserror::Error;
use xxhash_rust::xxh64::xxh64;

use crate::artifact::header::COMPILER_BUILDID;
use crate::artifact::model::ArchivedBundle;

use super::header::{Header, MAGIC, VERSION};
use super::model::Bundle;

#[derive(Error, Debug)]
pub enum LoadError {
    #[error("IO error: {0}")]
    Io(#[from] io::Error),
    #[error("Invalid magic bytes. Expected 'PDLA', got {0:?}")]
    InvalidMagic([u8; 4]),
    #[error("Invalid build ID: expected {expected}, got {actual}")]
    BuildIdMismatch { expected: u64, actual: u64 },
    #[error("Version mismatch: file v{file}, runtime v{runtime}")]
    VersionMismatch { file: u32, runtime: u32 },
    #[error("Checksum mismatch: expected {expected:x}, got {calculated:x}")]
    ChecksumMismatch { expected: u64, calculated: u64 },
    #[error("File too short")]
    Truncated,
}

pub struct LoadedBundle<'a> {
    pub archived: &'a Archived<Bundle>,
}

pub fn load_bundle<'a>(
    data: &'a [u8],
    build_id: Option<u64>,
) -> Result<LoadedBundle<'a>, LoadError> {
    let build_id = build_id.unwrap_or(COMPILER_BUILDID);

    if data.len() < 24 {
        return Err(LoadError::Truncated);
    }

    let (header_bytes, payload) = data.split_at(24);
    let header = Header::from_bytes(header_bytes.try_into().unwrap());

    if &header.magic != MAGIC {
        return Err(LoadError::InvalidMagic(header.magic));
    }

    if header.version != VERSION {
        return Err(LoadError::VersionMismatch {
            file: header.version,
            runtime: VERSION,
        });
    }

    if header.build_id != build_id {
        return Err(LoadError::BuildIdMismatch {
            expected: build_id,
            actual: header.build_id,
        });
    }

    let calculated = xxh64(payload, 0);
    if calculated != header.checksum {
        return Err(LoadError::ChecksumMismatch {
            expected: header.checksum,
            calculated,
        });
    }

    let archived = unsafe { rkyv::access_unchecked::<ArchivedBundle>(payload) };

    Ok(LoadedBundle { archived })
}
