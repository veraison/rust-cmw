// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! TN/CF conversion as per RFC 9277 (CoAP ↔ CBOR Tag mapping).

use thiserror::Error;

const CF_MAX: u16 = 65024;
const TN_MIN: u64 = 1668546817;
const TN_MAX: u64 = 1668612095;

#[derive(Debug, Error)]
pub enum TnError {
    #[error("C-F ID {0} out of range")]
    CfOutOfRange(u16),
    #[error("TN {0} out of range")]
    TnOutOfRange(u64),
}

/// Compute CBOR Tag number from CoAP Content‐Format ID.
pub fn tn(cf: u16) -> Result<u64, TnError> {
    if cf > CF_MAX {
        return Err(TnError::CfOutOfRange(cf));
    }
    let cf64 = cf as u64;
    Ok(TN_MIN + (cf64 / 255) * 256 + (cf64 % 255))
}

/// Compute CoAP Content‐Format ID from CBOR Tag number.
pub fn cf(tn: u64) -> Result<u16, TnError> {
    if !(TN_MIN..=TN_MAX).contains(&tn) {
        return Err(TnError::TnOutOfRange(tn));
    }
    let delta = tn - TN_MIN;
    let cf_calc = ((delta / 256) * 255) + (delta % 256);
    Ok(cf_calc as u16)
}

#[cfg(test)]
mod tests {
    use super::*;

    const CF_MIN: u16 = 0;
    #[test]
    fn test_tn_roundtrip() {
        for cf in CF_MIN..CF_MAX {
            let tn = tn(cf).unwrap();
            let cf2 = super::cf(tn).unwrap();
            assert_eq!(cf2, cf);
        }
    }

    #[test]
    fn test_tn_out_of_range() {
        assert!(tn(65535).is_err());
    }

    #[test]
    fn test_cf_out_of_range() {
        assert!(cf(TN_MIN - 1).is_err());
        for tn in TN_MIN..=TN_MAX {
            assert!(cf(tn).is_ok());
        }
        assert!(cf(TN_MAX + 1).is_err());
    }
}
