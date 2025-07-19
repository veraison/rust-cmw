// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Utility functions: URI/OID validation, JSON/CBOR detection, hex decoding.

/// CBOR/JSON sniffing: first byte → Format.
pub fn start_json_collection(c: u8) -> bool {
    c == b'{'
}
pub fn start_json_record(c: u8) -> bool {
    c == b'['
}
pub fn start_cbor_collection(c: u8) -> bool {
    (0xa0..=0xbb).contains(&c) || c == 0xbf
}
pub fn start_cbor_record(c: u8) -> bool {
    c == 0x82 || c == 0x83 || c == 0x9f
}
pub fn start_cbor_tag(c: u8) -> bool {
    c >= 0xda
}

/// Hex decode string (possibly multiline) into bytes.
pub fn hex_decode(s: &str) -> Result<Vec<u8>, hex::FromHexError> {
    let filtered: String = s.chars().filter(|c| !c.is_whitespace()).collect();
    hex::decode(filtered)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_start_functions() {
        assert!(start_json_collection(b'{'));
        assert!(start_json_record(b'['));
        assert!(start_cbor_collection(0xa1));
        assert!(start_cbor_record(0x82));
        assert!(start_cbor_tag(0xda));
    }
}
