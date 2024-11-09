use core::str::FromStr;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;


/// A JSON value (JavaScript Object Notation)
pub enum Json
{
	Array(Vec<Json>),
	Boolean(bool),
	Null,
	Number(f64),
	Object(BTreeMap<String, Json>),
	String(String),
}


enum Token
{
	ArrayBegin,
	ArrayEnd,
	Boolean(bool),
	Colon,
	Comma,
	Null,
	Number(f64),
	ObjectBegin,
	ObjectEnd,
	String(String),
}


impl Json
{
	/// Parse a JSON value in linear time if the data is valid JSON
	pub fn parse(bytes: &[u8]) -> Option<Json>
	{
		return parse(&mut tokenize(bytes)?);
	}
}


/// Tokenize the entire content, otherwise `None`
fn tokenize(bytes: &[u8]) -> Option<Vec<Token>>
{
	let mut tokens = Vec::<Token>::new();
	let mut i = 0;

	while i < bytes.len() {
		let byte = bytes[i];
		let mut token_len = 1;
		let token = match byte {
			b'\t' | b'\n' | b'\r' | b' ' => {
				i += 1;
				continue;
			},
			b'[' => Token::ArrayBegin,
			b']' => Token::ArrayEnd,
			b't' => match peek_keyword(&bytes[i..bytes.len()], b"true") {
				0 => return None,
				peeked_len => {
					token_len = peeked_len;
					Token::Boolean(true)
				},
			},
			b'f' => match peek_keyword(&bytes[i..bytes.len()], b"false") {
				0 => return None,
				peeked_len => {
					token_len = peeked_len;
					Token::Boolean(false)
				},
			},
			b':' => Token::Colon,
			b',' => Token::Comma,
			b'n' => match peek_keyword(&bytes[i..bytes.len()], b"null") {
				0 => return None,
				peeked_len => {
					token_len = peeked_len;
					Token::Null
				},
			},
			b'-' | b'0'..=b'9' => match peek_number(&bytes[i..bytes.len()]) {
				(0, _) => return None,
				(peeked_len, number) => {
					token_len = peeked_len;
					Token::Number(number)
				}
			},
			b'{' => Token::ObjectBegin,
			b'}' => Token::ObjectEnd,
			b'"' => match peek_string(&bytes[i..bytes.len()]) {
				(0, _) => return None,
				(peeked_len, string) => {
					token_len = peeked_len;
					Token::String(string)
				}
			},
			_ => return None,
		};
		i += token_len;
		tokens.push(token);
	}

	return Some(tokens);
}


/// Find the keyword at the start and return the bytes peeked, otherwise `0`
fn peek_keyword(remaining_bytes: &[u8], keyword: &[u8]) -> usize
{
	return match remaining_bytes.starts_with(keyword) {
		true => keyword.len(),
		false => 0,
	};
}


/// Find a JSON number at the start and return the bytes peeked and value,
/// otherwise `(0, 0)`
fn peek_number(remaining_bytes: &[u8]) -> (usize, f64)
{
	// Regular expression:
	// -?(0|1-9\d*)(\.\d+)?([eE][+-]?\d+)?

	enum State
	{
		Start,
		Negative,
		IntegerZero,
		IntegerNonZero,
		IntegerDigits,
		Dot,
		FractionDigits,
		E,
		Sign,
		ExponentDigits,
	}

	const INVALID_RESULT: (usize, f64) = (0, 0.0);

	let mut state = State::Start;
	let mut i = 0;

	for byte in remaining_bytes {
		state = match state {
			State::Start => match byte {
				b'-' => State::Negative,
				b'0' => State::IntegerZero,
				b'1' ..= b'9' => State::IntegerNonZero,
				_ => return INVALID_RESULT,
			},
			State::Negative => match byte {
				b'0' => State::IntegerZero,
				b'1' ..= b'9' => State::IntegerNonZero,
				_ => return INVALID_RESULT,
			},
			State::IntegerZero => match byte {
				b'.' => State::Dot,
				b'e' | b'E' => State::E,
				_ => break,
			},
			State::IntegerNonZero => match byte {
				b'0' ..= b'9' => State::IntegerDigits,
				b'.' => State::Dot,
				b'e' | b'E' => State::E,
				_ => break,
			},
			State::IntegerDigits => match byte {
				b'0' ..= b'9' => State::IntegerDigits,
				b'.' => State::Dot,
				b'e' | b'E' => State::E,
				_ => break,
			},
			State::Dot => match byte {
				b'0' ..= b'9' => State::FractionDigits,
				_ => return INVALID_RESULT,
			},
			State::FractionDigits => match byte {
				b'0' ..= b'9' => State::FractionDigits,
				b'e' | b'E' => State::E,
				_ => break,
			},
			State::E => match byte {
				b'+' | b'-' => State::Sign,
				b'0' ..= b'9' => State::ExponentDigits,
				_ => return INVALID_RESULT,
			},
			State::Sign => match byte {
				b'0' ..= b'9' => State::ExponentDigits,
				_ => return INVALID_RESULT,
			},
			State::ExponentDigits => match byte {
				b'0' ..= b'9' => State::ExponentDigits,
				_ => break,
			},
		};
		i += 1;
	}

	return match f64::from_str(unsafe { core::str::from_utf8_unchecked(&remaining_bytes[0..i]) }) {
		Ok(number) => (i, number),
		Err(_) => (0, 0.0),
	};
}


/// Find a JSON string at the start and return the bytes peeked and value,
/// otherwise `(0, String::new())`
fn peek_string(remaining_bytes: &[u8]) -> (usize, String)
{
	const BACKSPACE_CHAR: u8 = 8;
	const FORM_FEED_CHAR: u8 = 12;

	const INVALID_RESULT: (usize, String) = (0, String::new());

	let mut i: usize = 0;
	let mut result = Vec::<u8>::new();

	let len = remaining_bytes.len();
	while i < len {
		match remaining_bytes[i] {
			// Control characters
			0 ..= 31 => return INVALID_RESULT,
			// Quote
			b'"' => {
				if i > 0 {
					i += 1;
					break;
				}
				i += 1;
			},
			// Escape sequence
			b'\\' => {
				i += 1;
				match remaining_bytes.get(i) {
					Some(b'"') => result.push(b'"'),
					Some(b'\\') => result.push(b'\\'),
					Some(b'b') => result.push(BACKSPACE_CHAR),
					Some(b'f') => result.push(FORM_FEED_CHAR),
					Some(b'n') => result.push(b'\n'),
					Some(b'r') => result.push(b'\r'),
					Some(b't') => result.push(b'\t'),
					Some(b'u') => {
						// Convert the 4 hex characters to a code point
						let mut code_point: u32 = 0;
						const ASCII_ZERO: u32 = 48;
						const ASCII_UPPER_A: u32 = 65;
						const ASCII_LOWER_A: u32 = 97;
						const SHIFTS: [u32; 4] = [12, 8, 4, 0];
						for shift in SHIFTS {
							i += 1;
							match remaining_bytes.get(i) {
								Some(&byte @ b'0'..=b'9') => code_point += (byte as u32 - ASCII_ZERO) << shift,
								Some(&byte @ b'A'..=b'F') => code_point += (byte as u32 - ASCII_UPPER_A + 10) << shift,
								Some(&byte @ b'a'..=b'f') => code_point += (byte as u32 - ASCII_LOWER_A + 10) << shift,
								_ => return INVALID_RESULT,
							}
						}
						// Convert the code point to UTF-8 bytes
						let c = unsafe { char::from_u32_unchecked(code_point) };
						let mut buffer: [u8; 3] = [0, 0, 0];
						for &byte in c.encode_utf8(&mut buffer).as_bytes() {
							result.push(byte);
						}
					},
					_ => return INVALID_RESULT,
				}
				i += 1;
			},
			// Any other byte
			byte => {
				result.push(byte);
				i += 1;
			},
		}
	}

	return match String::from_utf8(result) {
		Ok(result) => (i, result),
		Err(_) => INVALID_RESULT,
	};
}


/// Get a pointer to the JSON value, assuming it's an Array
unsafe fn get_vec(value: &mut Json) -> *mut Vec<Json>
{
	return match value {
		Json::Array(array) => array as *mut Vec<Json>,
		_ => unreachable!(),
	}
}


/// Get a pointer to the JSON value, assuming it's an Object
unsafe fn get_map(value: &mut Json) -> *mut BTreeMap<String, Json>
{
	return match value {
		Json::Object(object) => object as *mut BTreeMap<String, Json>,
		_ => unreachable!(),
	}
}


/// Parse the JSON value while consuming the strings already allocated,
/// otherwise `None`
fn parse(tokens: &mut [Token]) -> Option<Json>
{
	enum State
	{
		Start,
		ArrayBegin(*mut Vec<Json>),
		ArrayComma(*mut Vec<Json>),
		ArrayValue(*mut Vec<Json>),
		ObjectBegin(*mut BTreeMap<String, Json>),
		ObjectColon(*mut BTreeMap<String, Json>, *mut String),
		ObjectComma(*mut BTreeMap<String, Json>),
		ObjectKey(*mut BTreeMap<String, Json>, *mut String),
		ObjectValue(*mut BTreeMap<String, Json>),
		RootValue,
	}

	let mut root_value = Json::Null;

	let mut stack: Vec<State> = vec![State::Start];

	for token in tokens {
		match token {
			Token::ArrayBegin => match stack.last_mut() {
				Some(state) => match state {
					// [
					State::Start => {
						// Remember value
						root_value = Json::Array(Vec::new());
						// Replace state
						*state = State::RootValue;
						// Push state
						stack.push(State::ArrayBegin(unsafe { get_vec(&mut root_value) }));
					},
					// [ [
					// , [
					State::ArrayBegin(parent_array) | State::ArrayComma(parent_array) => {
						let parent_array = unsafe { &mut**parent_array };
						// Remember value
						parent_array.push(Json::Array(Vec::new()));
						// Replace state
						*state = State::ArrayValue(parent_array);
						// Push state
						let last_i = parent_array.len() - 1;
						let child_array = unsafe { get_vec(parent_array.get_unchecked_mut(last_i)) };
						stack.push(State::ArrayBegin(child_array));
					},
					// : [
					State::ObjectColon(object, key) => {
						let object = unsafe { &mut**object };
						let key = unsafe { &mut**key };
						// Remember value
						let array = match object.entry(core::mem::take(key)) {
							Entry::Occupied(_) => return None,
							Entry::Vacant(entry) => entry.insert(Json::Array(Vec::new())),
						};
						let array = unsafe { &mut*(array as *mut Json) };
						// Replace state
						*state = State::ObjectValue(object);
						// Push state
						let array = unsafe { get_vec(array) };
						stack.push(State::ArrayBegin(array));
					},
					_ => return None,
				},
				_ => return None,
			},
			Token::ArrayEnd => match stack.last() {
				Some(state) => match state {
					// [ ]
					State::ArrayBegin(_) => {
						// Pop state
						stack.pop();
					},
					// "array_value" ]
					State::ArrayValue(_) => {
						// Pop state
						stack.pop();
					},
					_ => return None,
				},
				_ => return None,
			},
			Token::Boolean(value) => match stack.last_mut() {
				Some(state) => match state {
					// true
					State::Start => {
						// Remember value
						root_value = Json::Boolean(*value);
						// Replace state
						*state = State::RootValue;
					},
					// [ true
					// , true
					State::ArrayBegin(array) | State::ArrayComma(array) => {
						let array = unsafe { &mut**array };
						// Remember value
						array.push(Json::Boolean(*value));
						// Replace state
						*state = State::ArrayValue(array);
					},
					// : true
					State::ObjectColon(object, key) => {
						let object = unsafe { &mut**object };
						let key = unsafe { &mut**key };
						// Remember value
						match object.insert(core::mem::take(key), Json::Boolean(*value)) {
							None => (),
							Some(_old_value) => return None,
						}
						// Replace state
						*state = State::ObjectValue(object);
					},
					_ => return None,
				},
				_ => return None,
			},
			Token::Colon => match stack.last_mut() {
				Some(state) => match state {
					// "key" :
					State::ObjectKey(object, key) => {
						let object = unsafe { &mut**object };
						let key = unsafe { &mut**key };
						// Replace state
						*state = State::ObjectColon(object, key);
					},
					_ => return None,
				},
				_ => return None,
			},
			Token::Comma => match stack.last_mut() {
				Some(state) => match state {
					// "array_value" ,
					State::ArrayValue(array) => {
						// Replace state
						*state = State::ArrayComma(*array);
					},
					// "object_value" ,
					State::ObjectValue(object) => {
						// Replace state
						*state = State::ObjectComma(*object);
					},
					_ => return None,
				},
				_ => return None,
			},
			Token::Null => match stack.last_mut() {
				Some(state) => match state {
					// null
					State::Start => {
						// Remember value
						root_value = Json::Null;
						// Replace state
						*state = State::RootValue;
					},
					// [ null
					// , null
					State::ArrayBegin(array) | State::ArrayComma(array) => {
						let array = unsafe { &mut**array };
						// Remember value
						array.push(Json::Null);
						// Replace state
						*state = State::ArrayValue(array);
					},
					// : null
					State::ObjectColon(object, key) => {
						let object = unsafe { &mut**object };
						let key = unsafe { &mut**key };
						// Remember value
						match object.insert(core::mem::take(key), Json::Null) {
							None => (),
							Some(_old_value) => return None,
						}
						// Replace state
						*state = State::ObjectValue(object);
					},
					_ => return None,
				},
				_ => return None,
			},
			Token::Number(value) => match stack.last_mut() {
				Some(state) => match state {
					// 123
					State::Start => {
						// Remember value
						root_value = Json::Number(*value);
						// Replace state
						*state = State::RootValue;
					},
					// [ 123
					// , 123
					State::ArrayBegin(array) | State::ArrayComma(array) => {
						let array = unsafe { &mut**array };
						// Remember value
						array.push(Json::Number(*value));
						// Replace state
						*state = State::ArrayValue(array);
					},
					// : 123
					State::ObjectColon(object, key) => {
						let object = unsafe { &mut**object };
						let key = unsafe { &mut**key };
						// Remember value
						match object.insert(core::mem::take(key), Json::Number(*value)) {
							None => (),
							Some(_old_value) => return None,
						}
						// Replace state
						*state = State::ObjectValue(object);
					},
					_ => return None,
				},
				_ => return None,
			},
			Token::ObjectBegin => match stack.last_mut() {
				Some(state) => match state {
					// {
					State::Start => {
						// Remember value
						root_value = Json::Object(BTreeMap::new());
						// Replace state
						*state = State::RootValue;
						// Push state
						stack.push(State::ObjectBegin(unsafe { get_map(&mut root_value) }));
					},
					// [ {
					// , {
					State::ArrayBegin(parent) | State::ArrayComma(parent) => {
						let parent = unsafe { &mut**parent };
						// Remember value
						parent.push(Json::Object(BTreeMap::new()));
						// Replace state
						*state = State::ArrayValue(parent);
						// Push state
						let last_i = parent.len() - 1;
						let object = unsafe { get_map(parent.get_unchecked_mut(last_i)) };
						stack.push(State::ObjectBegin(object));
					},
					// : {
					State::ObjectColon(parent_object, key) => {
						let parent_object = unsafe { &mut**parent_object };
						let key = unsafe { &mut**key };
						// Remember value
						let child_object = match parent_object.entry(core::mem::take(key)) {
							Entry::Occupied(_) => return None,
							Entry::Vacant(entry) => entry.insert(Json::Object(BTreeMap::new())),
						};
						let child_object = unsafe { &mut*(child_object as *mut Json) };
						// Replace state
						*state = State::ObjectValue(parent_object);
						// Push state
						let child_object = unsafe { get_map(child_object) };
						stack.push(State::ObjectBegin(child_object));
					},
					_ => return None,
				},
				_ => return None,
			},
			Token::ObjectEnd => match stack.last() {
				Some(state) => match state {
					// { }
					State::ObjectBegin(_) => {
						// Pop state
						stack.pop();
					},
					// "object_value" }
					State::ObjectValue(_) => {
						// Pop state
						stack.pop();
					},
					_ => return None,
				},
				_ => return None,
			},
			Token::String(value) => match stack.last_mut() {
				Some(state) => match state {
					// "root_value"
					State::Start => {
						// Remember value
						root_value = Json::String(core::mem::take(value));
						// Replace state
						*state = State::RootValue;
					},
					// [ "array_value"
					// , "array_value"
					State::ArrayBegin(array) | State::ArrayComma(array) => {
						let array = unsafe { &mut**array };
						// Remember value
						array.push(Json::String(core::mem::take(value)));
						// Replace state
						*state = State::ArrayValue(array);
					},
					// : "object_value"
					State::ObjectColon(object, key) => {
						let object = unsafe { &mut**object };
						let key = unsafe { &mut**key };
						// Remember value
						match object.insert(core::mem::take(key), Json::String(core::mem::take(value))) {
							None => (),
							Some(_old_value) => return None,
						}
						// Replace state
						*state = State::ObjectValue(object);
					},
					// { "object_key"
					// , "object_key"
					State::ObjectBegin(object) | State::ObjectComma(object) => {
						let object = unsafe { &mut**object };
						// Replace state
						*state = State::ObjectKey(object, value);
					},
					_ => return None,
				},
				_ => return None,
			},
		}
	}

	return match stack.last_mut() {
		Some(State::RootValue) => Some(root_value),
		_ => None,
	};
}
