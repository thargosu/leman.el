// Integration tests for the leman-agent JSON protocol.
//
// Tests drive the real binary over stdio (line-delimited JSON), as
// Leman.el does.  The round-trip tests pair the agent (Bob) with an
// in-process matrix-sdk-crypto machine (Alice), acting as a virtual
// homeserver shuttling outgoing requests between the two.

use std::collections::BTreeMap;
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, Stdio};

use matrix_sdk_crypto::{
    types::events::room::encrypted::EncryptedEvent, types::requests::AnyOutgoingRequest,
    DecryptionSettings, EncryptionSyncChanges, OlmMachine, TrustRequirement,
};
use ruma::api::client::keys::upload_keys::v3::Response as UploadKeysResponse;
use ruma::events::AnyToDeviceEvent;
use serde_json::{json, Value};
use tempfile::TempDir;

struct TestAgent {
    child: Child,
    stdout: BufReader<std::process::ChildStdout>,
    next_id: u64,
}

impl TestAgent {
    fn spawn() -> Self {
        let mut child = Command::new(env!("CARGO_BIN_EXE_leman-agent"))
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .expect("failed to spawn agent");
        let stdout = child.stdout.take().unwrap();
        Self {
            child,
            stdout: BufReader::new(stdout),
            next_id: 1,
        }
    }

    fn send(&mut self, value: &Value) {
        let mut stdin = self.child.stdin.take().unwrap();
        writeln!(stdin, "{value}").unwrap();
        self.child.stdin = Some(stdin);
    }

    fn request(&mut self, cmd: &str, params: Value) -> Value {
        let id = self.next_id;
        self.next_id += 1;
        let mut message = json!({"id": id, "cmd": cmd});
        if !params.is_null() {
            message["params"] = params;
        }
        self.send(&message);
        self.recv_response(id)
    }

    fn recv_response(&mut self, id: u64) -> Value {
        let mut line = String::new();
        self.stdout.read_line(&mut line).expect("read response");
        let response: Value = serde_json::from_str(&line).expect("response is valid JSON");
        assert_eq!(response["id"], json!(id), "response id mismatch");
        response
    }
}

impl Drop for TestAgent {
    fn drop(&mut self) {
        // Ask the agent to exit cleanly first: a SIGKILLed process
        // never writes its coverage profile.
        if let Some(mut stdin) = self.child.stdin.take() {
            let _ = writeln!(stdin, r#"{{"id":999999,"cmd":"quit"}}"#);
            drop(stdin);
            for _ in 0..100 {
                if matches!(self.child.try_wait(), Ok(Some(_))) {
                    break;
                }
                std::thread::sleep(std::time::Duration::from_millis(20));
            }
        }
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

fn raw_from_value<T>(value: &Value) -> ruma::serde::Raw<T>
where
    T: serde::de::DeserializeOwned,
{
    let raw_value = serde_json::value::RawValue::from_string(value.to_string()).unwrap();
    ruma::serde::Raw::from_json(raw_value)
}

fn initialize_params(store: &TempDir) -> Value {
    json!({
        "user_id": "@bob:example.org",
        "device_id": "BOBDEVICE",
        "store_path": store.path().to_str().unwrap(),
    })
}

/// Run the full key exchange between Alice (in-process) and Bob (the
/// agent), then have Alice encrypt one event.  Returns the encrypted
/// event, ready for `decrypt_room_event`.
async fn exchange_and_encrypt(agent: &mut TestAgent, alice: &OlmMachine) -> Value {
    use matrix_sdk_crypto::{types::requests::AnyOutgoingRequest, EncryptionSettings};
    use ruma::{
        api::client::keys::{
            claim_keys::v3::Response as ClaimKeysResponse,
            get_keys::v3::Response as GetKeysResponse,
            upload_keys::v3::Response as UploadKeysResponse,
        },
        device_id,
        encryption::{DeviceKeys, OneTimeKey},
        events::AnyToDeviceEvent,
        serde::Raw,
        user_id,
    };

    // Bob uploads his keys; the virtual homeserver stores the upload
    // body and answers with an empty response.
    let bob_upload = agent.request("outgoing_requests", json!({}));
    let request = &bob_upload["ok"]["requests"][0];
    assert!(
        request["path"].as_str().unwrap().contains("/keys/upload"),
        "expected keys upload request: {request}"
    );
    // The body is a JSON string, passed through the client verbatim.
    assert!(
        request["body"].is_string(),
        "request body must be a JSON string: {request}"
    );
    let body: Value =
        serde_json::from_str(request["body"].as_str().unwrap()).expect("valid body JSON");
    let bob_device_keys = body["device_keys"].clone();
    let bob_one_time_keys = body["one_time_keys"].clone();
    agent.request(
        "mark_request_as_sent",
        json!({"request_id": request["id"], "response": {"one_time_key_counts": {}}}),
    );

    // Alice uploads her keys, tracks Bob, and queries his device keys.
    for request in alice.outgoing_requests().await.unwrap() {
        if matches!(request.request(), AnyOutgoingRequest::KeysUpload(_)) {
            alice
                .mark_request_as_sent(
                    request.request_id(),
                    &UploadKeysResponse::new(BTreeMap::new()),
                )
                .await
                .unwrap();
        }
    }
    alice
        .update_tracked_users(vec![user_id!("@bob:example.org")])
        .await
        .unwrap();
    for request in alice.outgoing_requests().await.unwrap() {
        if let AnyOutgoingRequest::KeysQuery(_) = request.request() {
            let mut response = GetKeysResponse::new();
            response.device_keys.insert(
                user_id!("@bob:example.org").to_owned(),
                {
                    let mut devices = BTreeMap::new();
                    devices.insert(
                        device_id!("BOBDEVICE").to_owned(),
                        serde_json::from_value::<Raw<DeviceKeys>>(bob_device_keys.clone()).unwrap(),
                    );
                    devices
                },
            );
            alice
                .mark_request_as_sent(request.request_id(), &response)
                .await
                .unwrap();
        }
    }

    // Alice claims one of Bob's one-time keys to establish an Olm session.
    let (claim_id, _claim_request) = alice
        .get_missing_sessions(vec![user_id!("@bob:example.org")].into_iter())
        .await
        .unwrap()
        .expect("should want to claim keys");
    let mut claim_response = ClaimKeysResponse::new(BTreeMap::new());
    claim_response.one_time_keys.insert(
        user_id!("@bob:example.org").to_owned(),
        {
            let mut devices = BTreeMap::new();
            let key_id = bob_one_time_keys.as_object().unwrap().keys().next().unwrap().clone();
            let key_id = serde_json::from_value::<ruma::OwnedOneTimeKeyId>(json!(key_id)).unwrap();
            let key_value = bob_one_time_keys.as_object().unwrap().values().next().unwrap().clone();
            devices.insert(
                device_id!("BOBDEVICE").to_owned(),
                {
                    let mut keys = BTreeMap::new();
                    keys.insert(key_id, serde_json::from_value::<Raw<OneTimeKey>>(key_value).unwrap());
                    keys
                },
            );
            devices
        },
    );
    alice
        .mark_request_as_sent(&claim_id, &claim_response)
        .await
        .unwrap();

    // Alice establishes a session with Bob and shares the room key.
    let room_id = ruma::room_id!("!room:example.org");
    let share_requests = alice
        .share_room_key(
            room_id,
            vec![user_id!("@bob:example.org")].into_iter(),
            EncryptionSettings::default(),
        )
        .await
        .unwrap();
    let mut room_key_events = Vec::new();
    for send in share_requests {
        for devices in send.messages.values() {
            for content in devices.values() {
                room_key_events.push(raw_from_value::<AnyToDeviceEvent>(&json!({
                    "sender": "@alice:example.org",
                    "type": serde_json::to_value(&send.event_type).unwrap(),
                    "content": serde_json::to_value(content).unwrap(),
                })));
            }
        }
    }
    assert!(!room_key_events.is_empty());

    // Bob receives the room key through the agent protocol.
    let decrypted_to_device = agent.request(
        "receive_sync_changes",
        json!({
            "to_device_events": room_key_events.iter().map(|e| serde_json::to_value(e).unwrap()).collect::<Vec<_>>(),
            "changed_devices": {"changed": [], "left": []},
            "one_time_keys_count": {"signed_curve25519": 100},
            "unused_fallback_keys": [],
            "next_batch_token": "s1",
        }),
    );
    let events = decrypted_to_device["ok"]["to_device_events"].as_array().unwrap();
    let key_event = events
        .iter()
        .find(|e| e["type"] == json!("m.room_key"))
        .expect("agent should return the decrypted room key");
    // The machine stores the key internally, but room keys are
    // zeroized when returned to the client (they must not leak into
    // client-visible JSON or logs).
    assert!(
        !key_event["content"]["session_id"].as_str().unwrap().is_empty(),
        "room key session id should be present"
    );
    assert_eq!(
        key_event["content"]["session_key"],
        json!(""),
        "room keys must be zeroized in processed to-device events"
    );

    // Alice encrypts an event.
    let content = json!({"body": "It's a secret to everybody.", "msgtype": "m.text"});
    let content_raw_value =
        serde_json::value::RawValue::from_string(content.to_string()).unwrap();
    let content_raw: ruma::serde::Raw<ruma::events::AnyMessageLikeEventContent> =
        ruma::serde::Raw::from_json(content_raw_value);
    let encrypted = alice
        .encrypt_room_event_raw(room_id, "m.room.message", &content_raw)
        .await
        .unwrap();
    json!({
        "sender": "@alice:example.org",
        "type": "m.room.encrypted",
        "origin_server_ts": 0,
        "event_id": "$fake",
        "room_id": room_id,
        "content": serde_json::to_value(encrypted.content).unwrap(),
    })
}

#[test]
fn test_hello() {
    let mut agent = TestAgent::spawn();
    let response = agent.request("hello", json!({}));
    assert_eq!(response["ok"]["protocol_version"], json!(1));
}

/// Respond to the agent's outgoing requests as a virtual homeserver.
/// `alice_keys`/`alice_otk` are Alice's published key uploads (used to
/// answer keys/query and keys/claim); returns the to-device messages
/// the agent wanted sent, as raw events for Alice's machine.
fn pump_agent(agent: &mut TestAgent, alice_keys: &Value, alice_otk: &Value) -> Vec<Value> {
    let mut sent_to_device = Vec::new();
    // The agent machine always tracks its own user, so keys/query
    // responses must also include its own device keys, or it will
    // re-query forever.
    let mut bob_device_keys = None;
    loop {
        let outgoing = agent.request("outgoing_requests", json!({}));
        let requests = outgoing["ok"]["requests"].as_array().unwrap().clone();
        if requests.is_empty() {
            break;
        }
        for request in requests {
            let path = request["path"].as_str().unwrap().to_owned();
            let response = if path.contains("/keys/upload") {
                // Report the number of one-time keys the server now
                // holds, else the machine keeps generating and
                // uploading more of them.
                let body: Value =
                    serde_json::from_str(request["body"].as_str().unwrap()).unwrap();
                bob_device_keys = Some(body["device_keys"].clone());
                let otk_count = body["one_time_keys"]
                    .as_object()
                    .map(|keys| keys.len())
                    .unwrap_or(0);
                json!({"one_time_key_counts": {"signed_curve25519": otk_count}})
            } else if path.contains("/keys/query") {
                let mut device_keys = json!({
                    "@alice:example.org": {"ALICEDEVICE": alice_keys},
                });
                if let Some(bob) = &bob_device_keys {
                    device_keys["@bob:example.org"] = json!({"BOBDEVICE": bob});
                }
                json!({"device_keys": device_keys, "failures": {}})
            } else if path.contains("/keys/claim") {
                let key_id = alice_otk
                    .as_object()
                    .unwrap()
                    .keys()
                    .next()
                    .unwrap()
                    .clone();
                let key_value = alice_otk.as_object().unwrap().values().next().unwrap().clone();
                json!({
                    "one_time_keys": {
                        "@alice:example.org": {"ALICEDEVICE": {key_id: key_value}},
                    },
                    "failures": {},
                })
            } else if path.contains("/sendToDevice") {
                let body: Value = serde_json::from_str(request["body"].as_str().unwrap()).unwrap();
                for user_messages in body["messages"].as_object().unwrap().values() {
                    if let Some(devices) = user_messages.as_object() {
                        for content in devices.values() {
                            sent_to_device.push(json!({
                                "sender": "@bob:example.org",
                                "type": "m.room.encrypted",
                                "content": content,
                            }));
                        }
                    }
                }
                json!({})
            } else {
                panic!("unexpected outgoing request path: {path}")
            };
            let mark = agent.request(
                "mark_request_as_sent",
                json!({"request_id": request["id"], "response": response}),
            );
            if mark.get("err").is_some() {
                eprintln!("pump: mark FAILED for {path}: {mark}");
            }
        }
    }
    sent_to_device
}

/// The agent encrypts an event for a room whose other member is Alice
/// (an in-process machine), and Alice decrypts it.  Exercises the
/// full E2 send flow: tracking, keys/claim dance, room key sharing,
/// and encryption, all through the protocol layer.
#[tokio::test]
async fn test_agent_encrypts_event() {
    // Alice publishes her keys through the harness.
    let alice = OlmMachine::new(
        ruma::user_id!("@alice:example.org"),
        ruma::device_id!("ALICEDEVICE"),
    )
    .await;
    let mut alice_keys = None;
    let mut alice_otk = None;
    for request in alice.outgoing_requests().await.unwrap() {
        if let AnyOutgoingRequest::KeysUpload(upload) = request.request() {
            alice_keys = Some(serde_json::to_value(&upload.device_keys).unwrap());
            alice_otk = Some(serde_json::to_value(&upload.one_time_keys).unwrap());
            alice
                .mark_request_as_sent(
                    request.request_id(),
                    &UploadKeysResponse::new(BTreeMap::new()),
                )
                .await
                .unwrap();
        }
    }
    let alice_keys = alice_keys.expect("alice should upload keys");
    let alice_otk = alice_otk.expect("alice should upload one-time keys");

    // The agent (Bob) tracks Alice and tries to encrypt.
    let mut agent = TestAgent::spawn();
    let store = TempDir::new().unwrap();
    let initialize = agent.request("initialize", initialize_params(&store));
    assert!(initialize["ok"].is_object());
    let bob_sender_key = initialize["ok"]["identity_keys"]["curve25519"]
        .as_str()
        .unwrap()
        .to_owned();
    agent.request(
        "update_tracked_users",
        json!({"users": ["@alice:example.org"]}),
    );

    let encrypt_params = json!({
        "room_id": "!room:example.org",
        "event_type": "m.room.message",
        "content": {"msgtype": "m.text", "body": "from the agent"},
        "users": ["@alice:example.org"],
    });
    // Pump first: the keys/query for Alice's devices (queued by
    // update_tracked_users) must complete before encrypting, otherwise
    // the key would be shared with nobody.
    let _sent = pump_agent(&mut agent, &alice_keys, &alice_otk);
    let response = agent.request("encrypt_room_event", encrypt_params.clone());
    // First encrypt attempt: Olm sessions with Alice's device are missing.
    assert_eq!(
        response["ok"]["status"],
        json!("claims_pending"),
        "expected claims_pending: {response}"
    );

    // Perform the pending requests (claim + others).
    let _sent = pump_agent(&mut agent, &alice_keys, &alice_otk);

    // Retry: now the agent can share a room key and encrypt.
    let response = agent.request("encrypt_room_event", encrypt_params.clone());
    let event = &response["ok"]["event"];
    assert_eq!(
        event["type"],
        json!("m.room.encrypted"),
        "expected an encrypted event: {response}"
    );

    // Perform the key-share to-device requests and deliver them to
    // Alice's machine.
    let sent = pump_agent(&mut agent, &alice_keys, &alice_otk);
    assert!(
        !sent.is_empty(),
        "the agent should share the room key with Alice's device"
    );
    let room_key_events: Vec<_> = sent
        .iter()
        .map(raw_from_value::<AnyToDeviceEvent>)
        .collect();
    alice
        .receive_sync_changes(
            EncryptionSyncChanges {
                to_device_events: room_key_events,
                changed_devices: &Default::default(),
                one_time_keys_counts: &Default::default(),
                unused_fallback_keys: None,
                next_batch_token: None,
            },
            &DecryptionSettings {
                sender_device_trust_requirement: TrustRequirement::Untrusted,
            },
        )
        .await
        .unwrap();

    // Alice decrypts the event the agent produced.
    let decrypted = alice
        .decrypt_room_event(
            &raw_from_value::<EncryptedEvent>(
                &json!({
                    "sender": "@bob:example.org",
                    "sender_key": bob_sender_key,
                    "type": "m.room.encrypted",
                    "event_id": "$fake",
                    "room_id": "!room:example.org",
                    "origin_server_ts": 0,
                    "content": event["content"].clone(),
                }),
            ),
            ruma::room_id!("!room:example.org"),
            &DecryptionSettings {
                sender_device_trust_requirement: TrustRequirement::Untrusted,
            },
        )
        .await
        .expect("alice should decrypt the agent's event");
    assert_eq!(
        serde_json::to_value(decrypted.event).unwrap()["content"]["body"],
        json!("from the agent")
    );
}

#[test]
fn test_framing_error() {
    let mut agent = TestAgent::spawn();
    // Malformed JSON.
    agent.send(&json!("{oops"));
    let mut line = String::new();
    agent.stdout.read_line(&mut line).expect("read response");
    let response: Value = serde_json::from_str(&line).unwrap();
    assert_eq!(response["err"]["code"], json!("parse"));
    // Valid JSON that is not an object.
    agent.send(&json!("not an object"));
    let mut line = String::new();
    agent.stdout.read_line(&mut line).expect("read response");
    let response: Value = serde_json::from_str(&line).unwrap();
    assert_eq!(response["err"]["code"], json!("parse"));
}

#[test]
fn test_unknown_command() {
    let mut agent = TestAgent::spawn();
    let response = agent.request("frobnicate", json!({}));
    assert_eq!(response["err"]["code"], json!("unknown_command"));
}

#[test]
fn test_initialize() {
    let mut agent = TestAgent::spawn();
    let store = TempDir::new().unwrap();
    let response = agent.request("initialize", initialize_params(&store));
    let ok = &response["ok"];
    assert!(ok["identity_keys"]["curve25519"].is_string());
    assert!(ok["identity_keys"]["ed25519"].is_string());
    assert_eq!(ok["user_id"], json!("@bob:example.org"));
}

/// The elisp side re-encodes sync data with `json-serialize', which
/// encodes absent sync fields (nil) as empty objects ({}), not null.
/// The agent must accept both forms for all optional sync fields.
#[tokio::test]
async fn test_receive_sync_changes_accepts_empty_objects() {
    let mut agent = TestAgent::spawn();
    let store = TempDir::new().unwrap();
    let initialize = agent.request("initialize", initialize_params(&store));
    assert!(initialize["ok"].is_object());

    let response = agent.request(
        "receive_sync_changes",
        json!({
            "to_device_events": [],
            "changed_devices": {},
            "one_time_keys_count": {},
            "unused_fallback_keys": {},
            "next_batch_token": "s1",
        }),
    );
    assert!(
        response["ok"].is_object(),
        "empty objects must be accepted: {response}"
    );

    let response = agent.request(
        "receive_sync_changes",
        json!({
            "to_device_events": [],
            "changed_devices": null,
            "one_time_keys_count": null,
            "unused_fallback_keys": null,
            "next_batch_token": "s2",
        }),
    );
    assert!(
        response["ok"].is_object(),
        "nulls must be accepted: {response}"
    );
}

/// The flagship round trip: Alice shares a room key with Bob (the
/// agent), then sends an encrypted event which the agent decrypts.
/// The test harness acts as the virtual homeserver.
#[tokio::test]
async fn test_alice_bob_round_trip() {
    let alice = OlmMachine::new(ruma::user_id!("@alice:example.org"), ruma::device_id!("ALICEDEVICE")).await;
    let mut agent = TestAgent::spawn();
    let store = TempDir::new().unwrap();
    let initialize = agent.request("initialize", initialize_params(&store));
    assert!(initialize["ok"].is_object());

    let encrypted_event = exchange_and_encrypt(&mut agent, &alice).await;
    let decrypted = agent.request(
        "decrypt_room_event",
        json!({"room_id": "!room:example.org", "event": encrypted_event}),
    );
    assert_eq!(
        decrypted["ok"]["event"]["content"]["body"],
        json!("It's a secret to everybody."),
    );
}

/// Sessions and identity keys must survive an agent restart: after
/// the key exchange with one agent process, a fresh agent process
/// with the same store decrypts the same event without receiving the
/// room key again.
#[tokio::test]
async fn test_restart_persistence() {
    let alice = OlmMachine::new(ruma::user_id!("@alice:example.org"), ruma::device_id!("ALICEDEVICE")).await;
    let store = TempDir::new().unwrap();
    let encrypted_event;
    let identity_keys;
    {
        let mut agent = TestAgent::spawn();
        let initialize = agent.request("initialize", initialize_params(&store));
        identity_keys = initialize["ok"]["identity_keys"].clone();
        encrypted_event = exchange_and_encrypt(&mut agent, &alice).await;
        let quit = agent.request("quit", json!({}));
        assert_eq!(quit["ok"]["bye"], json!(true));
    }

    // A fresh agent process reopens the same store.
    let mut agent = TestAgent::spawn();
    let initialize = agent.request("initialize", initialize_params(&store));
    assert_eq!(
        initialize["ok"]["identity_keys"],
        identity_keys,
        "identity keys must be stable across restarts"
    );
    let decrypted = agent.request(
        "decrypt_room_event",
        json!({"room_id": "!room:example.org", "event": encrypted_event}),
    );
    assert_eq!(
        decrypted["ok"]["event"]["content"]["body"],
        json!("It's a secret to everybody."),
        "sessions must survive an agent restart"
    );
}
