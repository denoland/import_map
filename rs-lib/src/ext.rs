// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use serde_json::json;
use serde_json::Value;
use url::Url;

/// This function can be used to modify the `import` mapping to expand
/// bare specifier imports to provide "directory" imports, eg.:
/// - `"express": "npm:express@4` -> `"express/": "npm:/express@4/`
/// - `"@std": "jsr:@std` -> `"std@/": "jsr:/@std/`
///
/// Only `npm:` and `jsr:` scheme are expanded and if there's already a
/// "directory" import, it is not overwritten.
pub fn expand_imports(import_map: ImportMapConfig) -> Value {
  let mut expanded_imports = serde_json::Map::new();

  let imports = import_map
    .import_map_value
    .get("imports")
    .cloned()
    .unwrap_or_else(|| Value::Null);

  if let Some(imports_map) = imports.as_object() {
    for (key, value) in imports_map {
      if !key.ends_with('/') {
        expanded_imports.insert(key.to_string(), value.clone());
        let key_with_trailing_slash = format!("{}/", key);

        // Don't overwrite existing keys
        if imports_map.contains_key(&key_with_trailing_slash) {
          continue;
        }

        let Some(value_str) = value.as_str() else {
          continue;
        };

        if !value_str.ends_with('/') {
          let value_with_trailing_slash =
            if let Some(value_str) = value_str.strip_prefix("jsr:") {
              let value_str = value_str.strip_prefix('/').unwrap_or(value_str);
              Some(format!("jsr:/{}/", value_str))
            } else if let Some(value_str) = value_str.strip_prefix("npm:") {
              let value_str = value_str.strip_prefix('/').unwrap_or(value_str);
              Some(format!("npm:/{}/", value_str))
            } else {
              None
            };

          if let Some(value_with_trailing_slash) = value_with_trailing_slash {
            expanded_imports.insert(
              key_with_trailing_slash,
              Value::String(value_with_trailing_slash),
            );
            continue;
          }
        }
      }

      expanded_imports.insert(key.to_string(), value.clone());
    }
  }

  Value::Object(expanded_imports)
}

pub struct ImportMapConfig {
  pub base_url: Url,
  pub import_map_value: Value,
}

fn pop_last_segment(url: &Url) -> Url {
  let mut path = url.to_file_path().unwrap();
  path.pop();
  Url::from_directory_path(path).unwrap()
}

pub fn create_synthetic_import_map(
  base_import_map: ImportMapConfig,
  children_import_maps: Vec<ImportMapConfig>,
) -> Option<(Url, Value)> {
  let mut synth_import_map_imports = serde_json::Map::new();
  let mut synth_import_map_scopes = serde_json::Map::new();

  let base_import_map_dir = pop_last_segment(&base_import_map.base_url);

  for child_config in children_import_maps.iter() {
    let mut member_scope = serde_json::Map::new();

    let member_dir = pop_last_segment(&child_config.base_url);
    let relative_to_base_dir =
      base_import_map_dir.make_relative(&member_dir).unwrap();
    let member_prefix = format!(
      "./{}/",
      relative_to_base_dir
        .strip_suffix('/')
        .unwrap_or(&relative_to_base_dir)
    );

    if let Some(imports) = child_config.import_map_value.get("imports") {
      for (key, value) in imports.as_object().unwrap() {
        let Some(value_str) = value.as_str() else {
          continue;
        };
        let value_url = member_dir.join(value_str).unwrap();
        // `make_relative` can fail here if the provided value is already
        // a fully resolved URL.
        let value_relative_to_base_dir =
          match base_import_map_dir.make_relative(&value_url) {
            Some(v) => format!("./{}", v),
            None => value_str.to_string(),
          };
        member_scope
          .insert(key.to_string(), Value::String(value_relative_to_base_dir));
      }
    }
    synth_import_map_scopes
      .insert(member_prefix.to_string(), Value::Object(member_scope));

    if let Some(scopes) = child_config.import_map_value.get("scopes") {
      for (scope_name, scope_obj) in scopes.as_object().unwrap() {
        // Keys for scopes need to be processed - they might look like
        // "/foo/" and coming from "bar" workspace member. So we need to
        // prepend the member name to the scope.
        let Ok(scope_name_dir) = member_dir.join(scope_name) else {
          synth_import_map_scopes.insert(scope_name.clone(), scope_obj.clone());
          continue; // not a file specifier
        };
        let Some(relative_to_base_dir) =
          base_import_map_dir.make_relative(&scope_name_dir)
        else {
          synth_import_map_scopes.insert(scope_name.clone(), scope_obj.clone());
          continue; // not a file specifier
        };
        let new_key = format!(
          "./{}/",
          relative_to_base_dir
            .strip_suffix('/')
            .unwrap_or(&relative_to_base_dir)
        );

        let mut new_scope = serde_json::Map::new();
        for (key, value) in scope_obj.as_object().unwrap() {
          let Some(value_str) = value.as_str() else {
            continue;
          };
          let value_url = member_dir.join(value_str).unwrap();
          // `make_relative` can fail here if the provided value is already
          // a fully resolved URL.
          let value_relative_to_base_dir =
            match base_import_map_dir.make_relative(&value_url) {
              Some(v) => format!("./{}", v),
              None => value_str.to_string(),
            };
          new_scope
            .insert(key.to_string(), Value::String(value_relative_to_base_dir));
        }
        synth_import_map_scopes.insert(new_key, Value::Object(new_scope));
      }
    }
  }

  if let Some(base_imports) = base_import_map.import_map_value.get("imports") {
    let base_imports_obj = base_imports.as_object().unwrap();
    for (key, value) in base_imports_obj.iter() {
      synth_import_map_imports.insert(key.to_owned(), value.to_owned());
    }
  }
  if let Some(base_scopes) = base_import_map.import_map_value.get("scopes") {
    let base_scopes_obj = base_scopes.as_object().unwrap();
    for (key, value) in base_scopes_obj.iter() {
      synth_import_map_scopes.insert(key.to_owned(), value.to_owned());
    }
  }

  let mut import_map = json!({});

  if !synth_import_map_imports.is_empty() {
    import_map["imports"] = Value::Object(synth_import_map_imports);
  }
  if !synth_import_map_scopes.is_empty() {
    import_map["scopes"] = Value::Object(synth_import_map_scopes);
  }

  if !import_map.as_object().unwrap().is_empty() {
    Some((base_import_map.base_url, import_map))
  } else {
    None
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use pretty_assertions::assert_eq;

  #[test]
  fn test_synthetic_import_map() {
    let base_import_map = ImportMapConfig {
      base_url: Url::parse("file:///import_map.json").unwrap(),
      import_map_value: json!({
        "imports": {
          "@std/assert/": "deno:/@std/assert/",
          "express": "npm:express@4"
        },
        "scopes": {}
      }),
    };
    let children = vec![
      ImportMapConfig {
        base_url: Url::parse("file:///foo/deno.json").unwrap(),
        import_map_value: json!({
          "imports": {
            "moment": "npm:moment@5",
          },
          "scopes": {},
        }),
      },
      ImportMapConfig {
        base_url: Url::parse("file:///bar/deno.json").unwrap(),
        import_map_value: json!({
          "imports": {
            "express": "npm:express@4",
          },
          "scopes": {},
        }),
      },
    ];
    let (url, value) =
      create_synthetic_import_map(base_import_map, children).unwrap();
    assert_eq!(url.as_str(), "file:///import_map.json");
    assert_eq!(
      value,
      json!({
        "imports": {
          "@std/assert/": "deno:/@std/assert/",
          "express": "npm:express@4"
        },
        "scopes": {
          "./foo/": {
            "moment": "npm:moment@5"
          },
          "./bar/": {
            "express": "npm:express@4"
          },
        },
      })
    );
  }

  #[test]
  fn test_synthetic_import_map2() {
    let base_import_map = ImportMapConfig {
      base_url: Url::parse("file:///import_map.json").unwrap(),
      import_map_value: json!({
        "imports": {
          "@std/assert/": "deno:/@std/assert/",
          "express": "npm:express@4"
        },
        "scopes": {
          "https://raw.githubusercontent.com/example/": {
            "https://deno.land/std/": "https://deno.land/std@0.177.0/"
          }
        }
      }),
    };
    let children = vec![
      ImportMapConfig {
        base_url: Url::parse("file:///foo/deno.json").unwrap(),
        import_map_value: json!({
          "imports": {
            "~/": "./",
            "foo/": "./bar/"
          },
          "scopes": {
            "./fizz/": {
              "express": "npm:express@5",
            },
            "https://raw.githubusercontent.com/other/": {
              "https://deno.land/std/": "https://deno.land/std@0.177.0/"
            }
          },
        }),
      },
      ImportMapConfig {
        base_url: Url::parse("file:///bar/deno.json").unwrap(),
        import_map_value: json!({
          "imports": {
            "@/": "./",
            "secret_mod/": "./some_mod/"
          },
          "scopes": {
            "./fizz/": {
              "foo/": "./buzz/",
            }
          },
        }),
      },
    ];
    let (url, value) =
      create_synthetic_import_map(base_import_map, children).unwrap();
    assert_eq!(url.as_str(), "file:///import_map.json");
    assert_eq!(
      value,
      json!({
        "imports": {
          "@std/assert/": "deno:/@std/assert/",
          "express": "npm:express@4"
        },
        "scopes": {
          "./foo/": {
            "~/": "./foo/",
            "foo/": "./foo/bar/"
          },
          "./foo/fizz/": {
            "express": "npm:express@5",
          },
          "./bar/": {
            "@/": "./bar/",
            "secret_mod/": "./bar/some_mod/"
          },
          "./bar/fizz/": {
            "foo/": "./bar/buzz/"
          },
          "https://raw.githubusercontent.com/other/": {
            "https://deno.land/std/": "https://deno.land/std@0.177.0/"
          },
          "https://raw.githubusercontent.com/example/": {
            "https://deno.land/std/": "https://deno.land/std@0.177.0/",
          },
        },
      })
    );
  }

  #[test]
  fn test_expand_imports() {
    let import_map = ImportMapConfig {
      base_url: Url::parse("file:///import_map.json").unwrap(),
      import_map_value: json!({
        "imports": {
          "@std": "jsr:/@std",
          "@foo": "jsr:@foo",
          "express": "npm:express@4",
          "foo": "https://example.com/foo/bar"
        },
        "scopes": {}
      }),
    };
    let value = expand_imports(import_map);
    assert_eq!(
      value,
      json!({
        "@std": "jsr:/@std",
        "@std/": "jsr:/@std/",
        "@foo": "jsr:@foo",
        "@foo/": "jsr:/@foo/",
        "express": "npm:express@4",
        "express/": "npm:/express@4/",
        "foo": "https://example.com/foo/bar"
      })
    );
  }

  #[test]
  fn test_expand_imports_with_trailing_slash() {
    let import_map = ImportMapConfig {
      base_url: Url::parse("file:///import_map.json").unwrap(),
      import_map_value: json!({
        "imports": {
          "express": "npm:express@4",
          "express/": "npm:/express@4/foo/bar/",
        },
        "scopes": {}
      }),
    };
    let value = expand_imports(import_map);
    assert_eq!(
      value,
      json!({
        "express": "npm:express@4",
        "express/": "npm:/express@4/foo/bar/",
      })
    );
  }
}
