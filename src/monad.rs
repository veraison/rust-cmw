// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Monad CMW: record or CBOR Tag, with JSON/CBOR (de)serialization.

use crate::{
    cmw::Error,
    indicator::Indicator,
    r#type::Type,
    utils::{start_cbor_record, start_cbor_tag},
    value::Value,
};
use mime::Mime;
use minicbor::{
    data::{Int, Tag},
    encode::Write,
    Decoder, Encoder,
};
use serde_json::Value as JsonValue;
use std::{fmt, str::FromStr};

/// Format enum: JSON/CBOR record, collection, or CBOR Tag.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Format {
    Json,
    CborRecord,
    CborTag,
}

impl fmt::Display for Format {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Format::Json => "JSON record",
            Format::CborRecord => "CBOR record",
            Format::CborTag => "CBOR tag",
        };
        write!(f, "{s}")
    }
}

/// Internal monad structure.
#[derive(Clone, Debug)]
pub struct Monad {
    typ: Type,
    val: Value,
    ind: Option<Indicator>,
    pub(crate) format: Option<Format>,
}

impl PartialEq for Monad {
    fn eq(&self, other: &Self) -> bool {
        self.typ == other.typ && self.val == other.val && self.ind == other.ind
    }
}

impl Monad {
    /// Create a new Monad with the given MIME media type, value, and optional indicator.
    pub fn new_media_type(
        media_type: Mime,
        value: Vec<u8>,
        ind: Option<Indicator>,
    ) -> Result<Self, Error> {
        let typ = Type::Media(media_type);
        let val = Value::new(value)?;
        let ind = if Some(Indicator::NONE) == ind {
            None
        } else {
            ind
        };
        Ok(Monad {
            typ,
            val,
            ind,
            format: None,
        })
    }

    /// Create a new Monad with the given CoAP Content-Format number, value, and optional indicator.
    pub fn new_cf(cf: u16, value: Vec<u8>, ind: Option<Indicator>) -> Result<Self, Error> {
        let typ = Type::CfNum(cf);
        let val = Value::new(value)?;
        let ind = if Some(Indicator::NONE) == ind {
            None
        } else {
            ind
        };
        Ok(Monad {
            typ,
            val,
            ind,
            format: None,
        })
    }

    /// Create a new Monad with the given CBOR Tag number, value, and optional indicator.
    pub fn new_tag(tn: u64, value: Vec<u8>, ind: Option<Indicator>) -> Result<Self, Error> {
        let typ = Type::TagNum(tn);
        let val = Value::new(value)?;
        let ind = if Some(Indicator::NONE) == ind {
            None
        } else {
            ind
        };
        Ok(Monad {
            typ,
            val,
            ind,
            format: None,
        })
    }

    pub fn type_(&self) -> String {
        self.typ.to_string()
    }

    pub fn value(&self) -> Vec<u8> {
        self.val.0.clone()
    }

    pub fn indicator(&self) -> Option<Indicator> {
        self.ind
    }

    pub fn use_cbor_tag_format(&mut self) {
        self.format = Some(Format::CborTag);
    }

    /// JSON serialization: [type, value, (indicator?)] array.
    pub fn to_json_value(&self) -> Result<JsonValue, Error> {
        let mut arr = Vec::new();
        // type
        let tval = serde_json::to_value(&self.typ).map_err(Error::Json)?;
        arr.push(tval);
        // value
        let vval = self.val.to_json_value()?;
        arr.push(vval);
        // indicator if nonzero
        if let Some(ind) = self.ind {
            let ival = serde_json::to_value(ind.bits()).map_err(Error::Json)?;
            arr.push(ival);
        }
        Ok(JsonValue::Array(arr))
    }

    /// JSON deserialization.
    pub fn from_json_bytes(b: &[u8]) -> Result<Self, Error> {
        if b.is_empty() {
            return Err(Error::InvalidData("empty buffer".into()));
        }
        let v: JsonValue = serde_json::from_slice(b)?;
        Self::from_json_value(&v)
    }

    /// Construct using [serde_json::Value]
    pub fn from_json_value(v: &JsonValue) -> Result<Self, Error> {
        let arr = v
            .as_array()
            .ok_or_else(|| Error::InvalidData("want monad to be a json record".into()))?;
        let alen = arr.len();
        if !(2..=3).contains(&alen) {
            return Err(Error::InvalidData(format!(
                "wrong number of entries ({alen}) in the CMW record"
            )));
        }
        // type
        let typ: Type = serde_json::from_value(arr[0].clone()).map_err(Error::Json)?;
        // value
        let val: Value = Value::from_json_value(&arr[1])?;
        let ind = if alen == 3 {
            let ind_num: u8 = serde_json::from_value(arr[2].clone()).map_err(Error::Json)?;
            Some(Indicator::from_bits_truncate(ind_num))
        } else {
            None
        };
        let format = Some(Format::Json);
        Ok(Monad {
            typ,
            val,
            ind,
            format,
        })
    }

    /// CBOR serialization.
    pub fn marshal_cbor(&self) -> Result<Vec<u8>, Error> {
        let buf = Vec::new();
        let mut encoder = Encoder::new(buf);

        self.marshal_cbor_to_encoder(&mut encoder)?;
        Ok(encoder.into_writer())
    }

    pub(crate) fn marshal_cbor_to_encoder<W: Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> Result<(), Error> {
        match self.format {
            // `Format::Json` is provenance metadata (the monad was parsed from
            // JSON); it must not prevent CBOR encoding. A JSON monad has the same
            // shape as a CBOR record, so encode it as one.
            Some(Format::CborRecord | Format::Json) | None => {
                self.marshal_cbor_record_to_encoder(encoder)
            }
            Some(Format::CborTag) => self.marshal_cbor_tag_to_encoder(encoder),
        }
    }

    pub(crate) fn marshal_cbor_record_to_encoder<W: Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> Result<(), Error> {
        // [type, value, (indicator?)]
        let len = if self.ind.is_none() { 2 } else { 3 };
        encoder
            .array(len)
            .or(Err(Error::CborEncode("writing CBOR array".to_string())))?;
        // type
        self.typ.marshal_cbor(encoder)?;
        // value
        encoder
            .bytes(&self.val.0)
            .or(Err(Error::CborEncode("writing CBOR value".to_string())))?;
        // indicator
        if let Some(ind) = self.ind {
            encoder
                .int(Int::from(ind.bits()))
                .or(Err(Error::CborEncode("writing CBOR indicator".to_string())))?;
        }
        Ok(())
    }

    pub(crate) fn marshal_cbor_tag_to_encoder<W: Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> Result<(), Error> {
        // Encode tag
        encoder
            .tag(Tag::new(self.typ.tag_number()?))
            .or(Err(Error::CborEncode("writing CBOR tag".to_string())))?;
        // Encode value
        encoder
            .bytes(&self.value())
            .or(Err(Error::CborEncode("writing CBOR value".to_string())))?;
        Ok(())
    }

    /// CBOR deserialization.
    pub fn unmarshal_cbor(b: &[u8]) -> Result<Self, Error> {
        let mut decoder = Decoder::new(b);
        Self::unmarshal_from_cbor_decoder(&mut decoder)
    }

    pub(crate) fn unmarshal_from_cbor_decoder(decoder: &mut Decoder<'_>) -> Result<Self, Error> {
        let first = decoder.input()[decoder.position()];
        if start_cbor_record(first) {
            Monad::unmarshal_record_from_cbor_decoder(decoder)
        } else if start_cbor_tag(first) {
            Monad::unmarshal_tag_from_cbor_decoder(decoder)
        } else {
            Err(Error::InvalidData(
                "want CBOR map, CBOR array or CBOR Tag start symbols".into(),
            ))
        }
    }

    pub(crate) fn unmarshal_record_from_cbor_decoder(
        decoder: &mut Decoder<'_>,
    ) -> Result<Self, Error> {
        // Decode array
        let alen_opt = decoder
            .array()
            .map_err(|e| Error::CborDecode(format!("unmarshal CMW CBOR Tag: {e}")))?;
        if let Some(alen) = alen_opt {
            if !(2..=3).contains(&alen) {
                return Err(Error::InvalidData(format!(
                    "wrong number of entries ({alen}) in CMW record"
                )));
            }
        } else {
            return Err(Error::InvalidData(
                "unknown number of entries in CMW record".into(),
            ));
        }

        // type
        let cmw_type_datatype = decoder.datatype().map_err(|e| {
            Error::CborDecode(format!(
                "Failed to determine CMW value datatype from CBOR: {e}"
            ))
        })?;
        let typ = match cmw_type_datatype {
            minicbor::data::Type::U16 | minicbor::data::Type::U8 | minicbor::data::Type::Int => {
                Type::CfNum(
                    decoder
                        .int()
                        .map_err(|e| {
                            Error::CborDecode(format!("Failed to parse CMW type as integer: {e}"))
                        })?
                        .try_into()
                        .map_err(|e| {
                            Error::CborDecode(format!("Failed to convert CMW type to u16: {e}"))
                        })?,
                )
            }
            minicbor::data::Type::String => Type::Media(
                Mime::from_str(decoder.str().map_err(|e| {
                    Error::CborDecode(format!("Failed to parse CMW type as string: {e}"))
                })?)
                .map_err(|e| {
                    Error::CborDecode(format!("Could not parse CMW type as media type: {e}"))
                })?,
            ),
            _ => {
                return Err(Error::CborDecode(format!(
                    "unmarshaling type: expected u16 or str, got {:?}",
                    cmw_type_datatype
                )));
            }
        };

        // value
        let val = decoder
            .bytes()
            .map_err(|e| Error::CborDecode(format!("Failed to parse CMW value as bytes: {e}")))?
            .to_vec();
        let val = Value::new(val)?;
        // indicator
        let ind = if Some(3) == alen_opt {
            Indicator::from_bits(decoder.u8().map_err(|e| {
                Error::CborDecode(format!("Failed to parse CMW indicator as u8: {e}"))
            })?)
        } else {
            None
        };
        Ok(Monad {
            typ,
            val,
            ind,
            format: Some(Format::CborRecord),
        })
    }

    pub(crate) fn unmarshal_tag_from_cbor_decoder(
        decoder: &mut Decoder<'_>,
    ) -> Result<Self, Error> {
        // Decode tag
        let tv = decoder
            .tag()
            .map_err(|e| Error::CborDecode(format!("unmarshal CMW CBOR Tag: {e}")))?
            .as_u64();
        let val = decoder
            .bytes()
            .map_err(|e| Error::CborDecode(format!("unmarshal CMW CBOR Tag: {e}")))?
            .to_vec();
        Ok(Monad {
            typ: Type::from_tag(tv)?,
            val: Value::new(val)?,
            ind: None,
            format: Some(Format::CborTag),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn invalid_json_record() {
        let vals = [
            json!({"collection": ["monad.type/sample", "AAAA"]}),
            json!([]),
            json!(["monad.type/sample"]),
            json!(["monad.type/sample", "AAAA", 4, "wrong"]),
            json!(["monad.type/sample", "AAAA", "wrong"]),
            json!(r#"["monad.type/sample", "AAAA"]"#),
        ];
        for (i, val) in vals.iter().enumerate() {
            assert!(Monad::from_json_value(val).is_err(), "error at index {}", i);
        }
    }

    #[test]
    fn valid_json_record() {
        let vals = [
            serde_json::json!(["monad.type/sample", "AAAA"]),
            serde_json::json!(["monad.type/sample", "AAAA", 4]),
        ];
        for (i, val) in vals.iter().enumerate() {
            assert!(Monad::from_json_value(val).is_ok(), "error at index {}", i);
        }
    }
}
