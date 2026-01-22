use rkyv::rancor::Error;
use std::io::{self, Write};
use xxhash_rust::xxh64::xxh64;

use super::header::Header;
use super::model::Bundle;

pub fn write_bundle(
    program: &Bundle,
    writer: &mut impl Write,
    build_id: Option<u64>,
) -> io::Result<()> {
    let bytes = rkyv::to_bytes::<Error>(program)
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

    let checksum = xxh64(&bytes, 0);

    let header = Header::new(checksum, build_id);

    writer.write_all(&header.as_bytes())?;

    writer.write_all(&bytes)?;

    Ok(())
}
