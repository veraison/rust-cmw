// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

//! Monad CMW: record or CBOR Tag, with JSON/CBOR (de)serialization.

use crate::{
    indicator::Indicator,
    r#type::Type,
    utils::{start_cbor_record, start_cbor_tag, start_json_record},
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
    pub fn new(media_type: Mime, value: Vec<u8>, ind: Option<Indicator>) -> Result<Self, String> {
        let typ = Type::Media(media_type);
        let val = Value::new(value).map_err(|e| e.to_string())?;
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

    pub fn new_cf(cf: u16, value: Vec<u8>, ind: Option<Indicator>) -> Result<Self, String> {
        let typ = Type::CfNum(cf);
        let val = Value::new(value).map_err(|e| e.to_string())?;
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
    pub fn to_json_value(&self) -> Result<JsonValue, String> {
        let mut arr = Vec::new();
        // type
        let tval = serde_json::to_value(&self.typ).map_err(|e| e.to_string())?;
        arr.push(tval);
        // value
        let vval = serde_json::to_value(&self.val).map_err(|e| e.to_string())?;
        arr.push(vval);
        // indicator if nonzero
        if let Some(ind) = self.ind {
            let ival = serde_json::to_value(ind.bits()).map_err(|e| e.to_string())?;
            arr.push(ival);
        }
        Ok(JsonValue::Array(arr))
    }

    /// JSON deserialization.
    pub fn from_json_bytes(b: &[u8]) -> Result<Self, String> {
        if b.is_empty() {
            return Err("empty buffer".into());
        }
        if !start_json_record(b[0]) {
            return Err("want JSON object or JSON array start symbols".to_string());
        }
        let arr: Vec<JsonValue> =
            serde_json::from_slice(b).map_err(|e| format!("unmarshal CMW JSON record: {e}"))?;
        let alen = arr.len();
        if !(2..=3).contains(&alen) {
            return Err(format!(
                "wrong number of entries ({alen}) in the CMW record"
            ));
        }
        // type
        let typ: Type = serde_json::from_value(arr[0].clone())
            .map_err(|e| format!("unmarshaling type: {e}"))?;
        // value
        let val: Value = serde_json::from_value(arr[1].clone())
            .map_err(|e| format!("unmarshaling value: {e}"))?;
        let ind = if alen == 3 {
            let ind_num: u8 = serde_json::from_value(arr[2].clone())
                .map_err(|e| format!("unmarshaling indicator: {e}"))?;
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
    pub fn marshal_cbor(&self) -> Result<Vec<u8>, String> {
        let buf = Vec::new();
        let mut encoder = Encoder::new(buf);

        self.marshal_cbor_to_encoder(&mut encoder)
            .map_err(|e| format!("marshaling CMW to CBOR: {e}"))?;
        Ok(encoder.into_writer())
    }

    pub(crate) fn marshal_cbor_to_encoder<W: Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> Result<(), String> {
        match self.format {
            Some(Format::CborRecord) | None => self.marshal_cbor_record_to_encoder(encoder),
            Some(Format::CborTag) => self.marshal_cbor_tag_to_encoder(encoder),
            _ => Err("invalid format for monad".into()),
        }
    }

    pub(crate) fn marshal_cbor_record_to_encoder<W: Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> Result<(), String> {
        // [type, value, (indicator?)]
        let len = if self.ind.is_none() { 2 } else { 3 };
        encoder
            .array(len)
            .or(Err("writing CBOR array".to_string()))?;
        // type
        self.typ.marshal_cbor(encoder)?;
        // value
        encoder
            .bytes(&self.val.0)
            .or(Err("writing CBOR value".to_string()))?;
        // indicator
        if let Some(ind) = self.ind {
            encoder
                .int(Int::from(ind.bits()))
                .or(Err("writing CBOR indicator".to_string()))?;
        }
        Ok(())
    }

    pub(crate) fn marshal_cbor_tag_to_encoder<W: Write>(
        &self,
        encoder: &mut Encoder<W>,
    ) -> Result<(), String> {
        // Encode tag
        encoder
            .tag(Tag::new(self.typ.tag_number()?))
            .or(Err("writing CBOR tag".to_string()))?;
        // Encode value
        encoder
            .bytes(&self.value())
            .or(Err("writing CBOR value".to_string()))?;
        Ok(())
    }

    /// CBOR deserialization.
    pub fn unmarshal_cbor(b: &[u8]) -> Result<Self, String> {
        if b.is_empty() {
            return Err("empty buffer".into());
        }
        let first = b[0];
        if start_cbor_record(first) {
            let mut decoder = Decoder::new(b);
            // Decode array
            Monad::unmarshal_record_from_cbor_decoder(&mut decoder)
        } else if start_cbor_tag(first) {
            let mut decoder = Decoder::new(b);
            // Decode tag
            Monad::unmarshal_tag_from_cbor_decoder(&mut decoder)
        } else {
            Err(format!(
                "want CBOR map, CBOR array or CBOR Tag start symbols, got: 0x{:02x}",
                first
            ))
        }
    }

    pub(crate) fn unmarshal_from_cbor_decoder(decoder: &mut Decoder<'_>) -> Result<Self, String> {
        let first = decoder.input()[decoder.position()];
        if start_cbor_record(first) {
            Monad::unmarshal_record_from_cbor_decoder(decoder)
        } else if start_cbor_tag(first) {
            Monad::unmarshal_tag_from_cbor_decoder(decoder)
        } else {
            Err("want CBOR map, CBOR array or CBOR Tag start symbols".into())
        }
    }

    pub(crate) fn unmarshal_record_from_cbor_decoder(
        decoder: &mut Decoder<'_>,
    ) -> Result<Self, String> {
        // Decode array
        let alen_opt = decoder
            .array()
            .map_err(|e| format!("unmarshal CMW CBOR Tag: {e}"))?;
        if let Some(alen) = alen_opt {
            if !(2..=3).contains(&alen) {
                return Err(format!("wrong number of entries ({alen}) in CMW record"));
            }
        } else {
            return Err("unknown number of entries in CMW record".into());
        }

        // type
        let cmw_type_datatype = decoder
            .datatype()
            .map_err(|e| format!("Failed to determine CMW value datatype from CBOR: {e}"))?;
        let typ = match cmw_type_datatype {
            minicbor::data::Type::U16 | minicbor::data::Type::U8 | minicbor::data::Type::Int => {
                Type::CfNum(
                    decoder
                        .int()
                        .map_err(|e| format!("Failed to parse CMW type as integer: {e}"))?
                        .try_into()
                        .map_err(|e| format!("Failed to convert CMW type to u16: {e}"))?,
                )
            }
            minicbor::data::Type::String => Type::Media(
                Mime::from_str(
                    decoder
                        .str()
                        .map_err(|e| format!("Failed to parse CMW type as string: {e}"))?,
                )
                .map_err(|e| format!("Could not parse CMW type as media type: {e}"))?,
            ),
            _ => {
                return Err(format!(
                    "unmarshaling type: expected u16 or str, got {:?}",
                    cmw_type_datatype
                ));
            }
        };

        // value
        let val = decoder
            .bytes()
            .map_err(|e| format!("Failed to parse CMW value as bytes: {e}"))?
            .to_vec();
        let val = Value::new(val).map_err(|e| e.to_string())?;
        // indicator
        let ind = if Some(3) == alen_opt {
            Indicator::from_bits(
                decoder
                    .u8()
                    .map_err(|e| format!("Failed to parse CMW indicator as u8: {e}"))?,
            )
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
    ) -> Result<Self, String> {
        // Decode tag
        let tv = decoder
            .tag()
            .map_err(|e| format!("unmarshal CMW CBOR Tag: {e}"))?
            .as_u64();
        let val = decoder
            .bytes()
            .map_err(|e| format!("unmarshal CMW CBOR Tag: {e}"))?
            .to_vec();
        Ok(Monad {
            typ: Type::from_tag(tv)?,
            val: Value::new(val)?,
            ind: None,
            format: Some(Format::CborTag),
        })
    }
}
