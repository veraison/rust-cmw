// Copyright 2025 Contributors to the Veraison project.
// SPDX-License-Identifier: Apache-2.0

use cmw::{collection::Collection, monad::Monad, Indicator, Mime, CMW};
use std::{fs, str::FromStr}; // Make sure serde and serde_json are included in your Cargo.toml

#[test]
fn test_parse_json_collection() {
    // Read testdata file from the crate root
    let data = fs::read_to_string("testdata/collection-ok.json")
        .expect("Failed to read testdata/collection-ok.json");

    // Parse the file contents into a CMW instance.
    let cmw: CMW = CMW::unmarshal_json(data.as_bytes()).expect("Failed to parse JSON into CMW");

    println!("Parsed CMW: {:?}", cmw);
}

#[test]
fn test_parse_cbor_collection() {
    // Read testdata file as binary data
    let data = fs::read("testdata/collection-cbor-ok.cbor")
        .expect("Failed to read testdata/collection-cbor-ok.cbor");

    // Parse the file contents into a CMW instance.
    let cmw: CMW = CMW::unmarshal_cbor(&data).expect("Failed to parse CBOR into CMW");

    println!("Parsed CMW: {:?}", cmw);
}

#[test]
#[ignore = "Fails due to items being arranged in a different order"]
fn test_encode_cbor_collection() {
    // Read testdata file as binary data
    let data = fs::read("testdata/collection-cbor-ok.cbor")
        .expect("Failed to read testdata/collection-cbor-ok.cbor");

    let cmw_1 = Monad::new_media_type(
        Mime::from_str("application/signed-corim+cbor")
            .expect("Failed to create media type for CMW 1"),
        vec![0xd2, 0x84, 0x43, 0xa1, 0x01, 0x26, 0xa1],
        Some(Indicator::from_bits_truncate(3)),
    )
    .expect("Failed to create Monad");

    let mut cmw_2 =
        Monad::new_cf(30001, vec![0x23, 0x47, 0xda, 0x55], None).expect("Failed to create Monad 2");
    cmw_2.use_cbor_tag_format();

    let cmw_3 =
        Monad::new_cf(30001, vec![0x23, 0x47, 0xda, 0x55], None).expect("Failed to create Monad 3");

    let mut collection = Collection::new(None, None).expect("Failed to create new collection");
    collection
        .add_item(cmw::collection::Label::Uint(1), cmw_1.into())
        .expect("Failed to add item1");
    collection
        .add_item(cmw::collection::Label::Uint(2), cmw_2.into())
        .expect("Failed to add item2");
    collection
        .add_item(cmw::collection::Label::Str("s".into()), cmw_3.into())
        .expect("Failed to add item3");
    let cmw = CMW::Collection(collection);
    let data_encoded = cmw.marshal_cbor().expect("Failed to marshal CMW to CBOR");

    // Ensure the encoded data matches the original data
    assert_eq!(data, data_encoded, "Encoded data does not match original");
    println!("Encoded CMW: {:?}", data_encoded);
    println!("Parsed CMW: {:?}", data);
}

#[test]
fn test_parse_cbor_2_collection() {
    // Read testdata file as binary data
    let data = fs::read("testdata/collection-cbor-ok-2.cbor")
        .expect("Failed to read testdata/collection-cbor-ok-2.cbor");

    // Parse the file contents into a CMW instance.
    let cmw: CMW = CMW::unmarshal_cbor(&data).expect("Failed to parse CBOR into CMW");

    println!("Parsed CMW: {:?}", cmw);
}

#[test]
fn test_parse_cbor_mixed_keys_collection() {
    // Read testdata file as binary data
    let data = fs::read("testdata/collection-cbor-mixed-keys.cbor")
        .expect("Failed to read testdata/collection-cbor-mixed-keys.cbor");

    // Parse the file contents into a CMW instance.
    let cmw: CMW = CMW::unmarshal_cbor(&data).expect("Failed to parse CBOR into CMW");

    println!("Parsed CMW: {:?}", cmw);
}

#[test]
fn test_encode_cbor_mixed_keys_collection() {
    // Read testdata file as binary data
    let data = fs::read("testdata/collection-cbor-mixed-keys.cbor")
        .expect("Failed to read testdata/collection-cbor-mixed-keys.cbor");

    let cmw_1 = Monad::new_cf(0, vec![0xff], None).expect("Failed to create Monad 1");
    let mut cmw_2 = Monad::new_cf(0, vec![0xaa], None).expect("Failed to create Monad 2");
    cmw_2.use_cbor_tag_format();
    let mut collection = Collection::new(None, None).expect("Failed to create new collection");
    collection
        .add_item(cmw::collection::Label::Str("string".into()), cmw_1.into())
        .expect("Failed to add item1");
    collection
        .add_item(cmw::collection::Label::Uint(1024), cmw_2.into())
        .expect("Failed to add item2");
    let cmw = CMW::Collection(collection);
    let data_encoded = cmw.marshal_cbor().expect("Failed to marshal CMW to CBOR");

    // Ensure the encoded data matches the original data
    assert_eq!(data, data_encoded, "Encoded data does not match original");
    println!("Encoded CMW: {:?}", data_encoded);
    println!("Parsed CMW: {:?}", data);
}
