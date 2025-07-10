// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Utility functions: URI/OID validation, JSON/CBOR detection, hex decoding.

use iri_string::types::UriStr;
use regex::Regex;

/// Validate collection type: must be a URI or an absolute OID.
pub fn validate_collection_type(s: &str) -> Result<(), String> {
    let oid_re = Regex::new(r"^([0-2])(([.]0)|([.][1-9][0-9]*))*$").unwrap();

    if oid_re.is_match(s) {
        return Ok(());
    }
    if UriStr::new(s).is_ok() {
        return Ok(());
    }
    Err(format!(
        "invalid collection type: {:?}. MUST be URI or OID",
        s
    ))
}

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
    fn test_validate_collection_type() {
        validate_collection_type("tag:example.com,2024:composite-attester").unwrap();
        assert!(validate_collection_type("urn:ietf:rfc:rfc9999").is_ok());
        assert!(validate_collection_type("http://www.ietf.org/rfc/rfc2396.txt").is_ok());
        assert!(validate_collection_type("1.2.3.4").is_ok());
        assert!(validate_collection_type("").is_err());
        assert!(validate_collection_type("a/b/c").is_err());
        assert!(validate_collection_type(".2.3.4").is_err());
    }

    #[test]
    fn test_start_functions() {
        assert!(start_json_collection(b'{'));
        assert!(start_json_record(b'['));
        assert!(start_cbor_collection(0xa1));
        assert!(start_cbor_record(0x82));
        assert!(start_cbor_tag(0xda));
    }
}
