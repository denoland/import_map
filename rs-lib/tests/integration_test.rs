// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use import_map::parse_from_json;
use import_map::parse_from_value_with_options;
use import_map::ImportMapOptions;
use import_map::ImportMapWithDiagnostics;
use pretty_assertions::assert_eq;
use serde_json::json;
use serde_json::Value;
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
        Some(Url::parse(base_url.as_str().unwrap()).unwrap())
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

    let maybe_import_map = if let Some(import_map) = test_obj.get("importMap") {
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
        let import_map =
          parse_from_json(&test.import_map_base_url, &test.import_map)
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
          assert!(parse_from_json(&test.import_map_base_url, &test.import_map)
            .is_err());
        } else {
          let import_map =
            parse_from_json(&test.import_map_base_url, &test.import_map)
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
  assert!(parse_from_json(&base_url, "{}").is_ok());

  let non_object_strings = vec!["null", "true", "1", "\"foo\"", "[]"];

  // invalid JSON
  for non_object in non_object_strings.iter().copied() {
    assert!(parse_from_json(&base_url, non_object).is_err());
  }

  // invalid JSON message test
  assert_eq!(
    parse_from_json(&base_url, "{\"a\":1,}")
      .unwrap_err()
      .to_string(),
    "Unable to parse import map JSON: trailing comma at line 1 column 8",
  );

  // invalid schema: 'imports' is non-object
  for non_object in non_object_strings.iter() {
    assert!(parse_from_json(
      &base_url,
      &format!("{{\"imports\": {}}}", non_object),
    )
    .is_err());
  }

  // invalid schema: 'scopes' is non-object
  for non_object in non_object_strings {
    assert!(parse_from_json(
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
  let result =
    parse_from_json(&Url::parse("https://deno.land").unwrap(), json_map);
  assert!(result.is_ok());
  let diagnostics = result.unwrap().diagnostics;
  assert_eq!(diagnostics.len(), 2);
  assert_eq!(diagnostics[0].to_string(), "Invalid address \"[\\\"https://example.com/2\\\"]\" for the specifier key \"bar\". Addresses must be strings.");
  assert_eq!(diagnostics[1].to_string(), "Invalid address \"null\" for the specifier key \"fizz\". Addresses must be strings.");
}

#[test]
fn from_json_3() {
  let json_map = r#"{
    "imports": {
      "foo": "https://example.com/1",
      "bar": "",
      "npm/": "https://example.com/2",
      "": "https://example.com/3"
    }
  }"#;
  let result =
    parse_from_json(&Url::parse("https://deno.land").unwrap(), json_map);
  assert!(result.is_ok());
  let diagnostics = result.unwrap().diagnostics;
  assert_eq!(diagnostics.len(), 3);
  assert_eq!(
    diagnostics[0].to_string(),
    "Invalid address \"\" for the specifier key \"bar\"."
  );
  assert_eq!(diagnostics[1].to_string(), "Invalid target address \"https://example.com/2\" for package specifier \"npm/\". Package address targets must end with \"/\".");
  assert_eq!(
    diagnostics[2].to_string(),
    "Invalid empty string specifier."
  );
}

#[test]
fn lookup_imports() {
  let json_map = r#"{
    "imports": {
      "fs": "https://deno.land/x/std@0.147.0/node/fs.ts",
      "mod/": "https://deno.land/x/mod@1.0.0/",
      "/~/": "../std/"
    }
  }"#;
  let result = parse_from_json(
    &Url::parse("file://C:/a/import-map.json").unwrap(),
    json_map,
  );
  assert!(result.is_ok());
  let ImportMapWithDiagnostics {
    diagnostics,
    import_map,
  } = result.unwrap();
  assert!(diagnostics.is_empty());
  let referrer = Url::parse("file://C:/a/a.ts").unwrap();
  let specifier_a =
    Url::parse("https://deno.land/x/std@0.147.0/node/fs.ts").unwrap();
  let result = import_map.lookup(&specifier_a, &referrer);
  assert_eq!(result, Some("fs".to_string()));
  let specifier_b = Url::parse("https://deno.land/x/mod@1.0.0/lib.ts").unwrap();
  let result = import_map.lookup(&specifier_b, &referrer);
  assert_eq!(result, Some("mod/lib.ts".to_string()));
  let specifier_c = Url::parse("file://C:/std/testing/asserts.ts").unwrap();
  let result = import_map.lookup(&specifier_c, &referrer);
  assert_eq!(result, Some("/~/testing/asserts.ts".to_string()));
}

#[test]
fn lookup_scopes() {
  let json_map = r#"{
    "imports": {
      "fs": "https://deno.land/x/std@0.147.0/node/fs.ts",
      "mod/": "https://deno.land/x/mod@1.0.0/",
      "/~/": "../std/"
    },
    "scopes": {
      "file:///a/": {
        "node:fs": "https://deno.land/x/std@0.147.0/node/fs.ts",
        "lib/": "https://deno.land/x/mod@1.0.0/",
        "/std/": "../std/"
      }
    }
  }"#;
  let result = parse_from_json(
    &Url::parse("file:///a/import-map.json").unwrap(),
    json_map,
  );
  assert!(result.is_ok());
  let ImportMapWithDiagnostics {
    diagnostics,
    import_map,
  } = result.unwrap();
  assert!(diagnostics.is_empty());
  let referrer = Url::parse("file:///a/a.ts").unwrap();
  let specifier_a =
    Url::parse("https://deno.land/x/std@0.147.0/node/fs.ts").unwrap();
  let result = import_map.lookup(&specifier_a, &referrer);
  assert_eq!(result, Some("node:fs".to_string()));
  let specifier_b = Url::parse("https://deno.land/x/mod@1.0.0/lib.ts").unwrap();
  let result = import_map.lookup(&specifier_b, &referrer);
  assert_eq!(result, Some("lib/lib.ts".to_string()));
  let specifier_c = Url::parse("file:///std/testing/asserts.ts").unwrap();
  let result = import_map.lookup(&specifier_c, &referrer);
  assert_eq!(result, Some("/std/testing/asserts.ts".to_string()));
}

#[test]
fn invalid_top_level_key() {
  let json_map = r#"{
    "imports": {
      "foo": "https://example.com/1"
    },
    "baz": {
      "bar": "https://example.com/2"
    }
  }"#;
  let result =
    parse_from_json(&Url::parse("https://deno.land").unwrap(), json_map);
  assert!(result.is_ok());
  let diagnostics = result.unwrap().diagnostics;
  assert_eq!(diagnostics.len(), 1);
  assert_eq!(diagnostics[0].to_string(), "Invalid top-level key \"baz\". Only \"imports\" and \"scopes\" can be present.");
}

#[test]
fn invalid_scope() {
  let json_map = r#"{
    "imports": {
      "foo": "https://example.com/1"
    },
    "scopes": {
      "///bar": {
        "baz": "https://example.com/2"
      }
    }
  }"#;
  let result =
    parse_from_json(&Url::parse("https://deno.land").unwrap(), json_map);
  assert!(result.is_ok());
  let diagnostics = result.unwrap().diagnostics;
  assert_eq!(diagnostics.len(), 1);
  assert_eq!(diagnostics[0].to_string(), "Invalid scope \"///bar\" (parsed against base URL \"https://deno.land/\").");
}

#[test]
fn querystring() {
  let json_map = r#"{
    "imports": {
      "npm/": "https://esm.sh/"
    }
  }"#;
  let import_map =
    parse_from_json(&Url::parse("https://deno.land").unwrap(), json_map)
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
fn append_imports() {
  let json_map = r#"{
    "imports": {
      "fs": "https://example.com/1"
    }
  }"#;
  let mut import_map =
    parse_from_json(&Url::parse("https://deno.land").unwrap(), json_map)
      .unwrap()
      .import_map;
  import_map
    .imports_mut()
    .append(
      "assert".to_string(),
      "https://deno.land/std/node/assert.ts".to_string(),
    )
    .unwrap();
  import_map
    .imports_mut()
    .append(
      "child_process".to_string(),
      "https://deno.land/std/node/child_process.ts".to_string(),
    )
    .unwrap();
  assert_eq!(
    import_map
      .imports_mut()
      .append(
        "fs".to_string(),
        "https://deno.land/std/node/fs.ts".to_string(),
      )
      .err()
      .unwrap(),
    "\"fs\" already exists and is mapped to \"https://example.com/1\""
  );
  import_map
    .imports_mut()
    .append(
      "url".to_string(),
      "https://deno.land/std/node/url.ts".to_string(),
    )
    .unwrap();

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

#[test]
fn get_or_append_scope_mut() {
  let mut import_map =
    parse_from_json(&Url::parse("https://deno.land").unwrap(), "{}")
      .unwrap()
      .import_map;

  // test adding
  let other_scopes = import_map.get_or_append_scope_mut("/other").unwrap();
  other_scopes
    .append("test".to_string(), "/other/".to_string())
    .unwrap();

  // test adding with a key that will resolve to the same as the first one
  let other_scopes = import_map
    .get_or_append_scope_mut("https://deno.land/other")
    .unwrap();
  other_scopes
    .append("second".to_string(), "/other2/".to_string())
    .unwrap();

  // now add an empty scope
  import_map.get_or_append_scope_mut("/empty").unwrap();

  assert_eq!(
    import_map.to_json(),
    r#"{
  "scopes": {
    "/other": {
      "test": "/other/",
      "second": "/other2/"
    },
    "/empty": {
    }
  }
}
"#
  );
}

#[test]
fn import_keys() {
  let json_map = r#"{
    "imports": {
      "fs": "https://example.com/1",
      "https://example.com/example/": "https://example.com/2/",
      "/~/": "./lib/"
    }
  }"#;
  let import_map =
    parse_from_json(&Url::parse("https://deno.land").unwrap(), json_map)
      .unwrap()
      .import_map;
  assert_eq!(
    import_map.imports().keys().collect::<Vec<_>>(),
    vec!["https://example.com/example/", "https://deno.land/~/", "fs"]
  );
}

#[test]
pub fn outputs_import_map_as_json_empty() {
  let json = r#"{
}
"#;
  let import_map = parse_from_json(&Url::parse("file:///dir/").unwrap(), json)
    .unwrap()
    .import_map;
  assert_eq!(import_map.to_json(), json);
}

#[test]
pub fn outputs_import_map_as_json_imports_only() {
  let json = r#"{
  "imports": {
    "a": "./vendor/sub/"
  }
}
"#;
  let import_map = parse_from_json(&Url::parse("file:///dir/").unwrap(), json)
    .unwrap()
    .import_map;
  assert_eq!(import_map.to_json(), json);
}

#[test]
pub fn outputs_import_map_as_json_scopes_only() {
  let json = r#"{
  "scopes": {
    "./vendor/sub/": {
      "xyz": "./other/",
      "abc": "./test/"
    }
  }
}
"#;
  let import_map = parse_from_json(&Url::parse("file:///dir/").unwrap(), json)
    .unwrap()
    .import_map;
  assert_eq!(import_map.to_json(), json);
}

#[test]
pub fn outputs_import_map_as_json_imports_and_scopes() {
  let json = r#"{
  "imports": {
    "dba": "./other/",
    "b": "./vendor/",
    "c": "./vendor/sub/",
    "d": "./vendor/sub/test.ts",
    "/~/": "./lib/"
  },
  "scopes": {
    "./vendor/sub/": {
      "a": "./other/"
    },
    "./sub/": {
      "a": "./vendor/sub/"
    },
    "./sub2/": {
      "a": "./vendor/sub/",
      "b": "./other/"
    }
  }
}
"#;
  let import_map = parse_from_json(&Url::parse("file:///dir/").unwrap(), json)
    .unwrap()
    .import_map;
  assert_eq!(import_map.to_json(), json);
}

#[test]
fn parse_with_address_hook() {
  let result = parse_from_value_with_options(
    &Url::parse("file:///").unwrap(),
    json!({
      "imports": {
        "preact": "npm:preact",
        "preact/": "npm:preact/",
      }
    }),
    ImportMapOptions {
      address_hook: Some(Box::new(|address, _, _| {
        if address.ends_with('/')
          && address.starts_with("npm:")
          && !address.starts_with("npm:/")
        {
          return format!("npm:/{}", &address[4..]);
        }
        address.to_string()
      })),
    },
  )
  .unwrap();
  assert_eq!(result.diagnostics, vec![]);
  let referrer = Url::parse("file:///mod.ts").unwrap();
  assert_eq!(
    result.import_map.resolve("preact", &referrer).unwrap(),
    Url::parse("npm:preact").unwrap()
  );
  assert_eq!(
    result
      .import_map
      .resolve("preact/hooks", &referrer)
      .unwrap(),
    Url::parse("npm:/preact/hooks").unwrap()
  );
}

#[test]
fn parse_with_no_double_encode() {
  let result = parse_from_json(
    &Url::parse("file:///").unwrap(),
    &json!({
      "imports": {
        "./": "./"
      }
    })
    .to_string(),
  )
  .unwrap();
  assert_eq!(result.diagnostics, vec![]);
  let referrer = Url::parse("file:///mod.ts").unwrap();
  assert_eq!(
    result.import_map.resolve("./{name}.ts", &referrer).unwrap(),
    Url::parse("file:///%7Bname%7D.ts").unwrap()
  );
}
