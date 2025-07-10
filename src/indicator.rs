// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Indicator bit‐map implementation (flags for ReferenceValues, Endorsements, etc.)

use bitflags::bitflags;
use std::fmt;

bitflags! {
    pub struct Indicator: u8 {
        const REFERENCE_VALUES    = 1 << 0;
        const ENDORSEMENTS        = 1 << 1;
        const EVIDENCE            = 1 << 2;
        const ATTESTATION_RESULTS = 1 << 3;
        const TRUST_ANCHORS       = 1 << 4;
    }
}

impl Indicator {
    pub(crate) const NONE: Indicator = Indicator::empty();
}

impl fmt::Display for Indicator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut items = Vec::new();
        if self.contains(Indicator::REFERENCE_VALUES) {
            items.push("reference values");
        }
        if self.contains(Indicator::ENDORSEMENTS) {
            items.push("endorsements");
        }
        if self.contains(Indicator::EVIDENCE) {
            items.push("evidence");
        }
        if self.contains(Indicator::ATTESTATION_RESULTS) {
            items.push("attestation results");
        }
        // Note: Go `indMap` does not include TRUST_ANCHORS in the string mapping.
        items.sort_unstable();
        write!(f, "{}", items.join(", "))
    }
}

#[cfg(test)]
mod tests {
    use super::Indicator;

    #[test]
    fn test_indicator_misc() {
        let mut i = Indicator::empty();
        assert!(i.is_empty());
        assert!(!i.contains(Indicator::ATTESTATION_RESULTS));
        assert!(!i.contains(Indicator::REFERENCE_VALUES));
        assert!(!i.contains(Indicator::ENDORSEMENTS));
        assert!(!i.contains(Indicator::EVIDENCE));
        assert!(!i.contains(Indicator::TRUST_ANCHORS));

        i.insert(Indicator::ATTESTATION_RESULTS);
        assert!(i.contains(Indicator::ATTESTATION_RESULTS));
        assert!(!i.contains(Indicator::REFERENCE_VALUES));
        assert!(!i.contains(Indicator::ENDORSEMENTS));
        assert!(!i.contains(Indicator::EVIDENCE));
        assert!(!i.contains(Indicator::TRUST_ANCHORS));

        i.remove(Indicator::ATTESTATION_RESULTS);
        assert!(i.is_empty());

        i.insert(Indicator::ATTESTATION_RESULTS);
        assert!(!i.is_empty());
        i.toggle(Indicator::ATTESTATION_RESULTS);
        assert!(i.is_empty());

        i.insert(Indicator::ATTESTATION_RESULTS);
        i.insert(Indicator::REFERENCE_VALUES);
        i.insert(Indicator::EVIDENCE);
        i.insert(Indicator::ENDORSEMENTS);
        i.insert(Indicator::TRUST_ANCHORS);
        assert!(i.contains(Indicator::ATTESTATION_RESULTS));
        assert!(i.contains(Indicator::REFERENCE_VALUES));
        assert!(i.contains(Indicator::ENDORSEMENTS));
        assert!(i.contains(Indicator::EVIDENCE));
        assert!(i.contains(Indicator::TRUST_ANCHORS));
    }
}
