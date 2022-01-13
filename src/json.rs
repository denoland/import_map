use std::collections::HashSet;

use indexmap::IndexMap;
use serde_json::Map;
use serde_json::Value;
use url::Url;

use crate::ImportMap;
use crate::ImportMapError;
use crate::ImportMapWithDiagnostics;
use crate::UnresolvedScopesMap;
use crate::UnresolvedSpecifierMap;

impl ImportMap {
  pub fn from_json_with_diagnostics(
    base_url: &Url,
    json_string: &str,
  ) -> Result<ImportMapWithDiagnostics, ImportMapError> {
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

    let mut diagnostics = vec![];
    let imports = if v.get("imports").is_some() {
      match v["imports"].take() {
        Value::Object(imports_map) => {
          parse_specifier_map_json(imports_map, &mut diagnostics)
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
          parse_scopes_map_json(scopes_map, &mut diagnostics)?
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

    let map_with_diagnostics =
      ImportMap::new_with_diagnostics(base_url, imports, scopes)?;
    diagnostics.extend(map_with_diagnostics.diagnostics);

    Ok(ImportMapWithDiagnostics {
      import_map: map_with_diagnostics.import_map,
      diagnostics,
    })
  }
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

#[cfg(test)]
mod tests {
  use crate::SpecifierMap;

  use super::*;
  use std::collections::HashMap;
  use std::path::Path;
  use std::path::PathBuf;
  use url::Url;
  use walkdir::WalkDir;

  #[derive(Debug)]
  enum TestKind {
    Resolution {
      given_specifier: String,
      expected_specifier: Option<String>,
      base_url: String,
    },
    Parse {
      expected_import_map: Value,
    },
  }

  #[derive(Debug)]
  struct ImportMapTestCase {
    name: String,
    import_map: String,
    import_map_base_url: Url,
    kind: TestKind,
  }

  fn load_import_map_wpt_tests() -> Vec<String> {
    let mut found_test_files = vec![];
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let import_map_wpt_path =
      repo_root.join("wpt/import-maps/data-driven/resources");
    eprintln!("import map wpt path {:#?}", import_map_wpt_path);
    for entry in WalkDir::new(import_map_wpt_path)
      .contents_first(true)
      .into_iter()
      .filter_entry(|e| {
        eprintln!("entry {:#?}", e);
        if let Some(ext) = e.path().extension() {
          return ext.to_string_lossy() == "json";
        }
        false
      })
      .filter_map(|e| match e {
        Ok(e) => Some(e),
        _ => None,
      })
      .map(|e| PathBuf::from(e.path()))
    {
      found_test_files.push(entry);
    }

    let mut file_contents = vec![];

    for file in found_test_files {
      let content = std::fs::read_to_string(file).unwrap();
      file_contents.push(content);
    }

    file_contents
  }

  fn parse_import_map_tests(test_str: &str) -> Vec<ImportMapTestCase> {
    let json_file: serde_json::Value = serde_json::from_str(test_str).unwrap();
    let maybe_name = json_file
      .get("name")
      .map(|s| s.as_str().unwrap().to_string());
    return parse_test_object(&json_file, maybe_name, None, None, None, None);

    fn parse_test_object(
      test_obj: &Value,
      maybe_name_prefix: Option<String>,
      maybe_import_map: Option<String>,
      maybe_base_url: Option<String>,
      maybe_import_map_base_url: Option<Url>,
      maybe_expected_import_map: Option<Value>,
    ) -> Vec<ImportMapTestCase> {
      let maybe_import_map_base_url =
        if let Some(base_url) = test_obj.get("importMapBaseURL") {
          Some(Url::parse(&base_url.as_str().unwrap().to_string()).unwrap())
        } else {
          maybe_import_map_base_url
        };

      let maybe_base_url = if let Some(base_url) = test_obj.get("baseURL") {
        Some(base_url.as_str().unwrap().to_string())
      } else {
        maybe_base_url
      };

      let maybe_expected_import_map =
        if let Some(im) = test_obj.get("expectedParsedImportMap") {
          Some(im.to_owned())
        } else {
          maybe_expected_import_map
        };

      let maybe_import_map = if let Some(import_map) = test_obj.get("importMap")
      {
        Some(if import_map.is_string() {
          import_map.as_str().unwrap().to_string()
        } else {
          serde_json::to_string(import_map).unwrap()
        })
      } else {
        maybe_import_map
      };

      if let Some(nested_tests) = test_obj.get("tests") {
        let nested_tests_obj = nested_tests.as_object().unwrap();
        let mut collected = vec![];
        for (name, test_obj) in nested_tests_obj {
          let nested_name = if let Some(ref name_prefix) = maybe_name_prefix {
            format!("{}: {}", name_prefix, name)
          } else {
            name.to_string()
          };
          let parsed_nested_tests = parse_test_object(
            test_obj,
            Some(nested_name),
            maybe_import_map.clone(),
            maybe_base_url.clone(),
            maybe_import_map_base_url.clone(),
            maybe_expected_import_map.clone(),
          );
          collected.extend(parsed_nested_tests)
        }
        return collected;
      }

      let mut collected_cases = vec![];
      if let Some(results) = test_obj.get("expectedResults") {
        let expected_results = results.as_object().unwrap();
        for (given, expected) in expected_results {
          let name = if let Some(ref name_prefix) = maybe_name_prefix {
            format!("{}: {}", name_prefix, given)
          } else {
            given.to_string()
          };
          let given_specifier = given.to_string();
          let expected_specifier = expected.as_str().map(|str| str.to_string());

          let test_case = ImportMapTestCase {
            name,
            import_map_base_url: maybe_import_map_base_url.clone().unwrap(),
            import_map: maybe_import_map.clone().unwrap(),
            kind: TestKind::Resolution {
              given_specifier,
              expected_specifier,
              base_url: maybe_base_url.clone().unwrap(),
            },
          };

          collected_cases.push(test_case);
        }
      } else if let Some(expected_import_map) = maybe_expected_import_map {
        let test_case = ImportMapTestCase {
          name: maybe_name_prefix.unwrap(),
          import_map_base_url: maybe_import_map_base_url.unwrap(),
          import_map: maybe_import_map.unwrap(),
          kind: TestKind::Parse {
            expected_import_map,
          },
        };

        collected_cases.push(test_case);
      } else {
        eprintln!("unreachable {:#?}", test_obj);
        unreachable!();
      }

      collected_cases
    }
  }

  fn run_import_map_test_cases(tests: Vec<ImportMapTestCase>) {
    for test in tests {
      match &test.kind {
        TestKind::Resolution {
          given_specifier,
          expected_specifier,
          base_url,
        } => {
          let import_map = ImportMap::from_json_with_diagnostics(
            &test.import_map_base_url,
            &test.import_map,
          )
          .unwrap()
          .import_map;
          let maybe_resolved = import_map
            .resolve(given_specifier, &Url::parse(base_url).unwrap())
            .ok()
            .map(|url| url.to_string());
          assert_eq!(expected_specifier, &maybe_resolved, "{}", test.name);
        }
        TestKind::Parse {
          expected_import_map,
        } => {
          if matches!(expected_import_map, Value::Null) {
            assert!(ImportMap::from_json_with_diagnostics(
              &test.import_map_base_url,
              &test.import_map
            )
            .is_err());
          } else {
            let import_map = ImportMap::from_json_with_diagnostics(
              &test.import_map_base_url,
              &test.import_map,
            )
            .unwrap()
            .import_map;
            let import_map_value = serde_json::to_value(import_map).unwrap();
            assert_eq!(expected_import_map, &import_map_value, "{}", test.name);
          }
        }
      }
    }
  }

  #[test]
  fn wpt() {
    let test_file_contents = load_import_map_wpt_tests();
    eprintln!("Found test files {}", test_file_contents.len());

    for test_file in test_file_contents {
      let tests = parse_import_map_tests(&test_file);
      run_import_map_test_cases(tests);
    }
  }

  #[test]
  fn from_json_1() {
    let base_url = Url::parse("https://deno.land").unwrap();

    // empty JSON
    assert!(ImportMap::from_json_with_diagnostics(&base_url, "{}").is_ok());

    let non_object_strings = vec!["null", "true", "1", "\"foo\"", "[]"];

    // invalid JSON
    for non_object in non_object_strings.to_vec() {
      assert!(
        ImportMap::from_json_with_diagnostics(&base_url, non_object).is_err()
      );
    }

    // invalid JSON message test
    assert_eq!(
      ImportMap::from_json_with_diagnostics(&base_url, "{\"a\":1,}")
        .unwrap_err()
        .to_string(),
      "Unable to parse import map JSON: trailing comma at line 1 column 8",
    );

    // invalid schema: 'imports' is non-object
    for non_object in non_object_strings.to_vec() {
      assert!(ImportMap::from_json_with_diagnostics(
        &base_url,
        &format!("{{\"imports\": {}}}", non_object),
      )
      .is_err());
    }

    // invalid schema: 'scopes' is non-object
    for non_object in non_object_strings.to_vec() {
      assert!(ImportMap::from_json_with_diagnostics(
        &base_url,
        &format!("{{\"scopes\": {}}}", non_object),
      )
      .is_err());
    }
  }

  #[test]
  fn from_json_2() {
    let json_map = r#"{
      "imports": {
        "foo": "https://example.com/1",
        "bar": ["https://example.com/2"],
        "fizz": null
      }
    }"#;
    let result = ImportMap::from_json_with_diagnostics(
      &Url::parse("https://deno.land").unwrap(),
      json_map,
    );
    assert!(result.is_ok());
  }

  #[test]
  fn mapped_windows_file_specifier() {
    // from issue #11530
    let mut specifiers = SpecifierMap::new();
    specifiers.insert(
      "file:///".to_string(),
      Some(Url::parse("http://localhost/").unwrap()),
    );

    let resolved_specifier = ImportMap::resolve_imports_match(
      &specifiers,
      "file:///C:/folder/file.ts",
      None,
    )
    .unwrap()
    .unwrap();

    assert_eq!(
      resolved_specifier.as_str(),
      "http://localhost/C:/folder/file.ts"
    );
  }

  #[test]
  fn querystring() {
    let json_map = r#"{
      "imports": {
        "npm/": "https://esm.sh/"
      }
    }"#;
    let import_map = ImportMap::from_json_with_diagnostics(
      &Url::parse("https://deno.land").unwrap(),
      json_map,
    )
    .unwrap()
    .import_map;

    let resolved = import_map
      .resolve(
        "npm/postcss-modules@4.2.2?no-check",
        &Url::parse("https://deno.land").unwrap(),
      )
      .unwrap();
    assert_eq!(
      resolved.as_str(),
      "https://esm.sh/postcss-modules@4.2.2?no-check"
    );

    let resolved = import_map
      .resolve("npm/key/a?b?c", &Url::parse("https://deno.land").unwrap())
      .unwrap();
    assert_eq!(resolved.as_str(), "https://esm.sh/key/a?b?c");

    let resolved = import_map
      .resolve(
        "npm/postcss-modules@4.2.2?no-check#fragment",
        &Url::parse("https://deno.land").unwrap(),
      )
      .unwrap();
    assert_eq!(
      resolved.as_str(),
      "https://esm.sh/postcss-modules@4.2.2?no-check#fragment"
    );

    let resolved = import_map
      .resolve(
        "npm/postcss-modules@4.2.2#fragment",
        &Url::parse("https://deno.land").unwrap(),
      )
      .unwrap();
    assert_eq!(
      resolved.as_str(),
      "https://esm.sh/postcss-modules@4.2.2#fragment"
    );
  }

  #[test]
  fn update_imports() {
    let json_map = r#"{
      "imports": {
        "fs": "https://example.com/1"
      }
    }"#;
    let mut import_map = ImportMap::from_json_with_diagnostics(
      &Url::parse("https://deno.land").unwrap(),
      json_map,
    )
    .unwrap()
    .import_map;
    let mut mappings = HashMap::new();
    mappings.insert(
      "assert".to_string(),
      "https://deno.land/std/node/assert.ts".to_string(),
    );
    mappings.insert(
      "child_process".to_string(),
      "https://deno.land/std/node/child_process.ts".to_string(),
    );
    mappings.insert(
      "fs".to_string(),
      "https://deno.land/std/node/fs.ts".to_string(),
    );
    mappings.insert(
      "url".to_string(),
      "https://deno.land/std/node/url.ts".to_string(),
    );
    let diagnostics = import_map.update_imports(mappings).unwrap();
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(
      diagnostics[0],
      "\"fs\" already exists and is mapped to \"https://example.com/1\""
    );
    assert_eq!(
      import_map
        .resolve("assert", &Url::parse("https://deno.land").unwrap())
        .unwrap()
        .as_str(),
      "https://deno.land/std/node/assert.ts"
    );
    assert_eq!(
      import_map
        .resolve("child_process", &Url::parse("https://deno.land").unwrap())
        .unwrap()
        .as_str(),
      "https://deno.land/std/node/child_process.ts"
    );
    assert_eq!(
      import_map
        .resolve("fs", &Url::parse("https://deno.land").unwrap())
        .unwrap()
        .as_str(),
      "https://example.com/1"
    );
    assert_eq!(
      import_map
        .resolve("url", &Url::parse("https://deno.land").unwrap())
        .unwrap()
        .as_str(),
      "https://deno.land/std/node/url.ts"
    );
  }
}
