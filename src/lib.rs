// Copyright 2021-2022 the Deno authors. All rights reserved. MIT license.

use indexmap::IndexMap;
use serde_json::Map;
use serde_json::Value;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::error::Error;
use std::fmt;
use url::Url;

#[derive(Debug)]
pub enum ImportMapError {
  UnmappedBareSpecifier(String, Option<String>),
  Other(String),
}

impl fmt::Display for ImportMapError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      ImportMapError::UnmappedBareSpecifier(specifier, maybe_referrer) => write!(
        f,
        "Relative import path \"{}\" not prefixed with / or ./ or ../ and not in import map{}",
        specifier,
        match maybe_referrer {
          Some(referrer) => format!(" from \"{}\"", referrer),
          None => String::new(),
        }
      ),
      ImportMapError::Other(message) => f.pad(message),
    }
  }
}

impl Error for ImportMapError {}

// https://url.spec.whatwg.org/#special-scheme
const SPECIAL_PROTOCOLS: &[&str] =
  &["ftp", "file", "http", "https", "ws", "wss"];
fn is_special(url: &Url) -> bool {
  SPECIAL_PROTOCOLS.contains(&url.scheme())
}

#[derive(Debug, Clone)]
struct SpecifierMapValue {
  /// The original index in the file. Used to determine the order
  /// when writing out the import map.
  index: usize,
  /// The raw key if it differs from the actual key.
  raw_key: Option<String>,
  /// The raw value if it differs from the actual value.
  raw_value: Option<String>,
  maybe_address: Option<Url>,
}

struct RawKeyValue {
  key: String,
  value: Option<String>,
}

impl SpecifierMapValue {
  pub fn new(
    index: usize,
    raw: &RawKeyValue,
    normalized_key: &str,
    value: Option<Url>,
  ) -> Self {
    Self {
      index,
      // we don't store these to reduce memory usage
      raw_key: if raw.key == normalized_key {
        None
      } else {
        Some(raw.key.to_string())
      },
      raw_value: if value.as_ref().map(|v| v.as_str()) == raw.value.as_deref() {
        None
      } else {
        raw.value.as_ref().map(|v| v.to_string())
      },
      maybe_address: value,
    }
  }
}

impl serde::Serialize for SpecifierMapValue {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    if let Some(value) = &self.maybe_address {
      value.serialize(serializer)
    } else {
      serializer.serialize_none()
    }
  }
}

#[derive(Debug, Clone)]
struct ScopesMapValue {
  /// The original index in the file. Used to determine the order
  /// when writing out the import map.
  index: usize,
  /// The raw key if it differs from the actual key.
  raw_key: Option<String>,
  imports: SpecifierMap,
}

impl serde::Serialize for ScopesMapValue {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    self.imports.serialize(serializer)
  }
}

type SpecifierMap = IndexMap<String, SpecifierMapValue>;
type ScopesMap = IndexMap<String, ScopesMapValue>;
type UnresolvedSpecifierMap = IndexMap<String, Option<String>>;
type UnresolvedScopesMap = IndexMap<String, UnresolvedSpecifierMap>;

#[derive(Debug, Clone, serde::Serialize)]
pub struct ImportMap {
  #[serde(skip)]
  base_url: Url,

  imports: SpecifierMap,
  scopes: ScopesMap,
}

#[derive(Debug, Clone)]
pub struct ImportMapWithDiagnostics {
  pub import_map: ImportMap,
  pub diagnostics: Vec<String>,
}

impl ImportMap {
  /// Provide the keys of the `imports` property of the import map.
  pub fn imports_keys(&self) -> Vec<&str> {
    self.imports.keys().map(|k| k.as_str()).collect()
  }

  pub fn base_url(&self) -> &Url {
    &self.base_url
  }

  pub fn resolve(
    &self,
    specifier: &str,
    referrer: &Url,
  ) -> Result<Url, ImportMapError> {
    let as_url: Option<Url> = try_url_like_specifier(specifier, referrer);
    let normalized_specifier = if let Some(url) = as_url.as_ref() {
      url.to_string()
    } else {
      specifier.to_string()
    };

    let scopes_match = resolve_scopes_match(
      &self.scopes,
      &normalized_specifier,
      as_url.as_ref(),
      referrer.as_ref(),
    )?;

    // match found in scopes map
    if let Some(scopes_match) = scopes_match {
      return Ok(scopes_match);
    }

    let imports_match = resolve_imports_match(
      &self.imports,
      &normalized_specifier,
      as_url.as_ref(),
    )?;

    // match found in import map
    if let Some(imports_match) = imports_match {
      return Ok(imports_match);
    }

    // The specifier was able to be turned into a URL, but wasn't remapped into anything.
    if let Some(as_url) = as_url {
      return Ok(as_url);
    }

    Err(ImportMapError::UnmappedBareSpecifier(
      specifier.to_string(),
      Some(referrer.to_string()),
    ))
  }

  /// This is a non-standard method that allows to add
  /// more "imports" mappings to already existing import map.
  pub fn update_imports(
    &mut self,
    imports: HashMap<String, String>,
  ) -> Result<Vec<String>, ImportMapError> {
    let mut diagnostics = vec![];
    let start_index = self
      .imports
      .values()
      .map(|v| v.index)
      .max()
      .map(|max| max + 1)
      .unwrap_or(0);

    for (i, (key, value)) in imports.into_iter().enumerate() {
      let raw = RawKeyValue {
        key,
        value: Some(value),
      };
      let key = match normalize_specifier_key(
        &raw.key,
        &self.base_url,
        &mut diagnostics,
      ) {
        Some(s) => s,
        None => continue,
      };
      if let Some(import_value) = self.imports.get(&key) {
        let import_val = if let Some(value) = &import_value.maybe_address {
          value.as_str()
        } else {
          "<invalid value>"
        };
        diagnostics.push(format!(
          "\"{}\" already exists and is mapped to \"{}\"",
          key, import_val
        ));
        continue;
      }

      let address_url = match try_url_like_specifier(
        raw.value.as_ref().unwrap(),
        &self.base_url,
      ) {
        Some(url) => url,
        None => {
          diagnostics.push(format!(
            "Invalid address \"{}\" for the specifier key {:?}.",
            raw.value.unwrap(),
            key
          ));
          continue;
        }
      };
      self.imports.insert(
        key.to_string(),
        SpecifierMapValue::new(start_index + i, &raw, &key, Some(address_url)),
      );
    }

    // Sort in longest and alphabetical order.
    self.imports.sort_by(|k1, _v1, k2, _v2| match k1.cmp(k2) {
      Ordering::Greater => Ordering::Less,
      Ordering::Less => Ordering::Greater,
      // index map guarantees that there can't be duplicate keys
      Ordering::Equal => unreachable!(),
    });

    Ok(diagnostics)
  }

  /// Removes any imports or scopes referencing the provided folder in
  /// the import map.
  pub fn with_folder_removed(&self, folder: &Url) -> Self {
    fn filter_imports(imports: &SpecifierMap, path: &Url) -> SpecifierMap {
      imports
        .iter()
        .filter_map(|(key, value)| {
          if let Some(value) = &value.maybe_address {
            if value.as_str().starts_with(&path.as_str()) {
              return None;
            }
          }
          Some((key.to_owned(), value.to_owned()))
        })
        .collect::<SpecifierMap>()
    }

    let base_url = self.base_url().to_owned();
    let imports = filter_imports(&self.imports, folder);
    let scopes = self
      .scopes
      .iter()
      .filter_map(|(key, value)| {
        if let Ok(base) = base_url.join(key) {
          if base.as_str().starts_with(&folder.as_str()) {
            return None;
          }
        }
        // now filter out any entries
        let imports = filter_imports(&value.imports, folder);
        if imports.is_empty() {
          None
        } else {
          Some((
            key.to_owned(),
            ScopesMapValue {
              index: value.index,
              raw_key: value.raw_key.clone(),
              imports,
            },
          ))
        }
      })
      .collect::<ScopesMap>();

    Self {
      base_url,
      imports,
      scopes,
    }
  }

  /// Gets the import map as JSON text.
  pub fn to_json(&self) -> String {
    let mut w = String::new();

    w.push('{');

    if !self.imports.is_empty() {
      w.push('\n');
      w.push_str(r#"  "imports": {"#);
      write_imports(&mut w, &self.imports, 2);
      w.push_str("\n  }");
    }

    if !self.scopes.is_empty() {
      if !self.imports.is_empty() {
        w.push(',');
      }
      w.push('\n');
      w.push_str(r#"  "scopes": {"#);
      write_scopes(&mut w, &self.scopes);
      w.push_str("\n  }");
    }

    w.push_str("\n}\n");

    return w;

    fn write_imports(
      w: &mut String,
      imports: &SpecifierMap,
      indent_level: usize,
    ) {
      // sort based on how it originally appeared in the file
      let mut imports = imports.iter().collect::<Vec<_>>();
      imports.sort_by_key(|v| v.1.index);

      for (i, (key, value)) in imports.into_iter().enumerate() {
        w.push_str(if i > 0 { ",\n" } else { "\n" });
        let raw_key = value.raw_key.as_ref().unwrap_or(key);
        let raw_value = value
          .raw_value
          .as_deref()
          .or_else(|| value.maybe_address.as_ref().map(|a| a.as_str()));

        w.push_str(&"  ".repeat(indent_level));
        w.push_str(&format!(r#""{}": "#, escape_string(raw_key)));
        if let Some(value) = raw_value {
          w.push_str(&format!(r#""{}""#, escape_string(value)));
        } else {
          w.push_str("null");
        }
      }
    }

    fn write_scopes(w: &mut String, scopes: &ScopesMap) {
      // sort based on how the it originally appeared in the file
      let mut scopes = scopes.iter().collect::<Vec<_>>();
      scopes.sort_by_key(|v| v.1.index);

      for (i, (key, value)) in scopes.into_iter().enumerate() {
        w.push_str(if i > 0 { ",\n" } else { "\n" });
        let raw_key = value.raw_key.as_ref().unwrap_or(key);

        w.push_str(&format!(r#"    "{}": {{"#, escape_string(raw_key)));
        write_imports(w, &value.imports, 3);
        w.push_str("\n    }");
      }
    }

    fn escape_string(text: &str) -> String {
      text.replace('"', "\\\"")
    }
  }
}

pub fn parse_from_json(
  base_url: &Url,
  json_string: &str,
) -> Result<ImportMapWithDiagnostics, ImportMapError> {
  let mut diagnostics = vec![];
  let (imports, scopes) = parse_json(json_string, &mut diagnostics)?;
  let normalized_imports =
    parse_specifier_map(imports, base_url, &mut diagnostics);
  let normalized_scopes = parse_scope_map(scopes, base_url, &mut diagnostics)?;

  Ok(ImportMapWithDiagnostics {
    diagnostics,
    import_map: ImportMap {
      base_url: base_url.clone(),
      imports: normalized_imports,
      scopes: normalized_scopes,
    },
  })
}

fn parse_json(
  json_string: &str,
  diagnostics: &mut Vec<String>,
) -> Result<(UnresolvedSpecifierMap, UnresolvedScopesMap), ImportMapError> {
  let mut v: Value = match serde_json::from_str(json_string) {
    Ok(v) => v,
    Err(err) => {
      return Err(ImportMapError::Other(format!(
        "Unable to parse import map JSON: {}",
        err,
      )));
    }
  };

  match v {
    Value::Object(_) => {}
    _ => {
      return Err(ImportMapError::Other(
        "Import map JSON must be an object".to_string(),
      ));
    }
  }

  let imports = if v.get("imports").is_some() {
    match v["imports"].take() {
      Value::Object(imports_map) => {
        parse_specifier_map_json(imports_map, diagnostics)
      }
      _ => {
        return Err(ImportMapError::Other(
          "Import map's 'imports' must be an object".to_string(),
        ));
      }
    }
  } else {
    IndexMap::new()
  };

  let scopes = if v.get("scopes").is_some() {
    match v["scopes"].take() {
      Value::Object(scopes_map) => {
        parse_scopes_map_json(scopes_map, diagnostics)?
      }
      _ => {
        return Err(ImportMapError::Other(
          "Import map's 'scopes' must be an object".to_string(),
        ));
      }
    }
  } else {
    IndexMap::new()
  };

  let mut keys: HashSet<String> = v
    .as_object()
    .unwrap()
    .keys()
    .map(|k| k.to_string())
    .collect();
  keys.remove("imports");
  keys.remove("scopes");
  for key in keys {
    diagnostics.push(format!("Invalid top-level key \"{}\". Only \"imports\" and \"scopes\" can be present.", key));
  }

  Ok((imports, scopes))
}

/// Convert provided JSON map to key values
fn parse_specifier_map_json(
  json_map: Map<String, Value>,
  diagnostics: &mut Vec<String>,
) -> UnresolvedSpecifierMap {
  let mut map: IndexMap<String, Option<String>> = IndexMap::new();

  // Order is preserved because of "preserve_order" feature of "serde_json".
  for (specifier_key, value) in json_map.into_iter() {
    map.insert(specifier_key.clone(), match value {
      Value::String(address) => Some(address),
      _ => {
        diagnostics.push(format!("Invalid address {:#?} for the specifier key \"{}\". Addresses must be strings.", value, specifier_key));
        None
      }
    });
  }

  map
}

/// Convert provided JSON map to key value strings.
fn parse_scopes_map_json(
  scopes_map: Map<String, Value>,
  diagnostics: &mut Vec<String>,
) -> Result<UnresolvedScopesMap, ImportMapError> {
  let mut map = UnresolvedScopesMap::new();

  // Order is preserved because of "preserve_order" feature of "serde_json".
  for (scope_prefix, mut potential_specifier_map) in scopes_map.into_iter() {
    let potential_specifier_map = match potential_specifier_map.take() {
      Value::Object(obj) => obj,
      _ => {
        return Err(ImportMapError::Other(format!(
          "The value for the {:?} scope prefix must be an object",
          scope_prefix
        )));
      }
    };

    let specifier_map =
      parse_specifier_map_json(potential_specifier_map, diagnostics);

    map.insert(scope_prefix.to_string(), specifier_map);
  }

  Ok(map)
}

/// Convert provided key value string imports to valid SpecifierMap.
///
/// From specification:
/// - order of iteration must be retained
/// - SpecifierMap's keys are sorted in longest and alphabetic order
fn parse_specifier_map(
  imports: UnresolvedSpecifierMap,
  base_url: &Url,
  diagnostics: &mut Vec<String>,
) -> SpecifierMap {
  let mut normalized_map: SpecifierMap = SpecifierMap::new();

  for (i, (key, value)) in imports.into_iter().enumerate() {
    let raw = RawKeyValue { key, value };
    let normalized_key =
      match normalize_specifier_key(&raw.key, base_url, diagnostics) {
        Some(s) => s,
        None => continue,
      };
    let potential_address = match &raw.value {
      Some(address) => address,
      None => {
        let value = SpecifierMapValue::new(i, &raw, &normalized_key, None);
        normalized_map.insert(normalized_key, value);
        continue;
      }
    };

    let address_url = match try_url_like_specifier(potential_address, base_url)
    {
      Some(url) => url,
      None => {
        diagnostics.push(format!(
          "Invalid address \"{}\" for the specifier key \"{}\".",
          potential_address, raw.key
        ));
        let value = SpecifierMapValue::new(i, &raw, &normalized_key, None);
        normalized_map.insert(normalized_key, value);
        continue;
      }
    };

    let address_url_string = address_url.to_string();
    if raw.key.ends_with('/') && !address_url_string.ends_with('/') {
      diagnostics.push(format!(
        "Invalid target address {:?} for package specifier {:?}. \
            Package address targets must end with \"/\".",
        address_url_string, raw.key
      ));
      let value = SpecifierMapValue::new(i, &raw, &normalized_key, None);
      normalized_map.insert(normalized_key, value);
      continue;
    }

    let value =
      SpecifierMapValue::new(i, &raw, &normalized_key, Some(address_url));
    normalized_map.insert(normalized_key, value);
  }

  // Sort in longest and alphabetical order.
  normalized_map.sort_by(|k1, _v1, k2, _v2| match k1.cmp(k2) {
    Ordering::Greater => Ordering::Less,
    Ordering::Less => Ordering::Greater,
    // JSON guarantees that there can't be duplicate keys
    Ordering::Equal => unreachable!(),
  });

  normalized_map
}

/// Convert provided key value string scopes to valid ScopeMap.
///
/// From specification:
/// - order of iteration must be retained
/// - ScopeMap's keys are sorted in longest and alphabetic order
fn parse_scope_map(
  scope_map: UnresolvedScopesMap,
  base_url: &Url,
  diagnostics: &mut Vec<String>,
) -> Result<ScopesMap, ImportMapError> {
  let mut normalized_map: ScopesMap = ScopesMap::new();

  // Order is preserved because of "preserve_order" feature of "serde_json".
  for (i, (raw_scope_prefix, potential_specifier_map)) in
    scope_map.into_iter().enumerate()
  {
    let scope_prefix_url = match base_url.join(&raw_scope_prefix) {
      Ok(url) => url.to_string(),
      _ => {
        diagnostics.push(format!(
          "Invalid scope \"{}\" (parsed against base URL \"{}\").",
          raw_scope_prefix, base_url
        ));
        continue;
      }
    };

    let norm_map =
      parse_specifier_map(potential_specifier_map, base_url, diagnostics);

    let value = ScopesMapValue {
      index: i,
      raw_key: if scope_prefix_url == raw_scope_prefix {
        None
      } else {
        // only store this if they differ to save memory
        Some(raw_scope_prefix)
      },
      imports: norm_map,
    };
    normalized_map.insert(scope_prefix_url, value);
  }

  // Sort in longest and alphabetical order.
  normalized_map.sort_by(|k1, _v1, k2, _v2| match k1.cmp(k2) {
    Ordering::Greater => Ordering::Less,
    Ordering::Less => Ordering::Greater,
    // JSON guarantees that there can't be duplicate keys
    Ordering::Equal => unreachable!(),
  });

  Ok(normalized_map)
}

fn try_url_like_specifier(specifier: &str, base: &Url) -> Option<Url> {
  if specifier.starts_with('/')
    || specifier.starts_with("./")
    || specifier.starts_with("../")
  {
    if let Ok(url) = base.join(specifier) {
      return Some(url);
    }
  }

  if let Ok(url) = Url::parse(specifier) {
    return Some(url);
  }

  None
}

/// Parse provided key as import map specifier.
///
/// Specifiers must be valid URLs (eg. "`https://deno.land/x/std/testing/asserts.ts`")
/// or "bare" specifiers (eg. "moment").
fn normalize_specifier_key(
  specifier_key: &str,
  base_url: &Url,
  diagnostics: &mut Vec<String>,
) -> Option<String> {
  // ignore empty keys
  if specifier_key.is_empty() {
    diagnostics.push("Invalid empty string specifier.".to_string());
    return None;
  }

  if let Some(url) = try_url_like_specifier(specifier_key, base_url) {
    return Some(url.to_string());
  }

  // "bare" specifier
  Some(specifier_key.to_string())
}

fn append_specifier_to_base(
  base: &Url,
  specifier: &str,
) -> Result<Url, url::ParseError> {
  let mut base = base.clone();
  let is_relative_or_absolute_specifier = specifier.starts_with("../")
    || specifier.starts_with("./")
    || specifier.starts_with('/');

  // The specifier could be a windows path such as "C:/a/test.ts" in which
  // case we don't want to use `join` because it will make the specifier
  // the url since it contains what looks to be a uri scheme. To work around
  // this, we append the specifier to the path segments of the base url when
  // the specifier is not relative or absolute.
  let mut maybe_query_string_and_fragment = None;
  if !is_relative_or_absolute_specifier && base.path_segments_mut().is_ok() {
    {
      let mut segments = base.path_segments_mut().unwrap();
      segments.pop_if_empty();

      // Handle query-string and fragment first, otherwise they would be percent-encoded
      // by `extend()`
      let prefix = match specifier.find(&['?', '#'][..]) {
        Some(idx) => {
          maybe_query_string_and_fragment = Some(&specifier[idx..]);
          &specifier[..idx]
        }
        None => specifier,
      };
      segments.extend(prefix.split('/'));
    }

    if let Some(query_string_and_fragment) = maybe_query_string_and_fragment {
      Ok(base.join(query_string_and_fragment)?)
    } else {
      Ok(base)
    }
  } else {
    Ok(base.join(specifier)?)
  }
}

fn resolve_scopes_match(
  scopes: &ScopesMap,
  normalized_specifier: &str,
  as_url: Option<&Url>,
  referrer: &str,
) -> Result<Option<Url>, ImportMapError> {
  // exact-match
  if let Some(scope_imports) = scopes.get(referrer) {
    let scope_match = resolve_imports_match(
      &scope_imports.imports,
      normalized_specifier,
      as_url,
    )?;
    // Return only if there was actual match (not None).
    if scope_match.is_some() {
      return Ok(scope_match);
    }
  }

  for (normalized_scope_key, scope_imports) in scopes.iter() {
    if normalized_scope_key.ends_with('/')
      && referrer.starts_with(normalized_scope_key)
    {
      let scope_match = resolve_imports_match(
        &scope_imports.imports,
        normalized_specifier,
        as_url,
      )?;
      // Return only if there was actual match (not None).
      if scope_match.is_some() {
        return Ok(scope_match);
      }
    }
  }

  Ok(None)
}

fn resolve_imports_match(
  specifier_map: &SpecifierMap,
  normalized_specifier: &str,
  as_url: Option<&Url>,
) -> Result<Option<Url>, ImportMapError> {
  // exact-match
  if let Some(value) = specifier_map.get(normalized_specifier) {
    if let Some(address) = &value.maybe_address {
      return Ok(Some(address.clone()));
    } else {
      return Err(ImportMapError::Other(format!(
        "Blocked by null entry for \"{:?}\"",
        normalized_specifier
      )));
    }
  }

  // Package-prefix match
  // "most-specific wins", i.e. when there are multiple matching keys,
  // choose the longest.
  for (specifier_key, value) in specifier_map.iter() {
    if !specifier_key.ends_with('/') {
      continue;
    }

    if !normalized_specifier.starts_with(specifier_key) {
      continue;
    }

    if let Some(url) = as_url {
      if !is_special(url) {
        continue;
      }
    }

    let resolution_result = value.maybe_address.as_ref().ok_or_else(|| {
      ImportMapError::Other(format!(
        "Blocked by null entry for \"{:?}\"",
        specifier_key
      ))
    })?;

    // Enforced by parsing.
    assert!(resolution_result.to_string().ends_with('/'));

    let after_prefix = &normalized_specifier[specifier_key.len()..];

    let url = match append_specifier_to_base(resolution_result, after_prefix) {
      Ok(url) => url,
      Err(_) => {
        return Err(ImportMapError::Other(format!(
          "Failed to resolve the specifier \"{:?}\" as its after-prefix
            portion \"{:?}\" could not be URL-parsed relative to the URL prefix
            \"{:?}\" mapped to by the prefix \"{:?}\"",
          normalized_specifier, after_prefix, resolution_result, specifier_key
        )));
      }
    };

    if !url.as_str().starts_with(resolution_result.as_str()) {
      return Err(ImportMapError::Other(format!(
        "The specifier \"{:?}\" backtracks above its prefix \"{:?}\"",
        normalized_specifier, specifier_key
      )));
    }

    return Ok(Some(url));
  }

  #[cfg(feature = "logging")]
  log::debug!(
    "Specifier {:?} was not mapped in import map.",
    normalized_specifier
  );

  Ok(None)
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn mapped_windows_file_specifier() {
    // from issue #11530
    let mut specifiers = SpecifierMap::new();
    specifiers.insert(
      "file:///".to_string(),
      SpecifierMapValue {
        index: 0,
        raw_key: None,
        raw_value: None,
        maybe_address: Some(Url::parse("http://localhost/").unwrap()),
      },
    );

    let resolved_specifier =
      resolve_imports_match(&specifiers, "file:///C:/folder/file.ts", None)
        .unwrap()
        .unwrap();

    assert_eq!(
      resolved_specifier.as_str(),
      "http://localhost/C:/folder/file.ts"
    );
  }
}
