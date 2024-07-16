// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use serde_json::json;
use serde_json::Value;
use url::Url;

use crate::import_map_list_resolve as import_map_list_resolve_;
use crate::ImportMap;
use crate::ImportMapError;

/// Resolves using a list of import maps. If a matching entry isn't found for
/// the given specifier and referrer, fallback to the next import map.
pub fn import_map_list_resolve<'a>(
  import_maps: impl IntoIterator<Item = &'a ImportMap>,
  specifier: &str,
  referrer: &Url,
) -> Result<Url, ImportMapError> {
  import_map_list_resolve_(import_maps, specifier, referrer)
}

/// This function can be used to modify the `imports` and "scopes" mappings
/// to expand bare specifier imports to provide "directory" imports, eg.:
/// - `"express": "npm:express@4` -> `"express/": "npm:/express@4/`
/// - `"@std": "jsr:@std` -> `"std@/": "jsr:/@std/`
///
/// Only `npm:` and `jsr:` scheme are expanded and if there's already a
/// "directory" import, it is not overwritten.
pub fn expand_import_map_value(import_map: Value) -> Value {
  let Value::Object(mut import_map) = import_map else {
    return import_map;
  };

  if let Some(imports) = import_map.get("imports").and_then(|i| i.as_object()) {
    import_map.insert(
      "imports".to_string(),
      Value::Object(expand_imports(imports)),
    );
  }
  if let Some(scopes) = import_map.remove("scopes") {
    match scopes {
      Value::Object(scopes) => {
        let mut expanded_scopes = serde_json::Map::with_capacity(scopes.len());
        for (key, imports) in scopes {
          let imports = match imports {
            Value::Object(imports) => Value::Object(expand_imports(&imports)),
            _ => imports,
          };
          expanded_scopes.insert(key, imports);
        }
        import_map.insert("scopes".to_string(), Value::Object(expanded_scopes));
      }
      _ => {
        import_map.insert("scopes".to_string(), scopes);
      }
    }
  }

  Value::Object(import_map)
}

fn expand_imports(
  imports_map: &serde_json::Map<String, Value>,
) -> serde_json::Map<String, Value> {
  let mut expanded_imports = serde_json::Map::new();
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
  expanded_imports
}

pub struct ImportMapConfig {
  pub base_url: Url,
  pub import_map_value: Value,
}

fn pop_last_segment(url: &Url) -> Url {
  let mut url = url.clone();
  if let Ok(mut segments) = url.path_segments_mut() {
    segments.pop();
  }
  // seems like a bug in the url crate that poping a path segment
  // does not maintain the slash separator
  // https://github.com/servo/rust-url/issues/912
  if !url.path().ends_with('/') {
    url.set_path(&format!("{}/", url.path()));
  }
  url
}

pub fn create_synthetic_import_map(
  mut base_import_map: ImportMapConfig,
  children_import_maps: Vec<ImportMapConfig>,
) -> (Url, Value) {
  let mut synth_import_map_imports = serde_json::Map::new();
  let mut synth_import_map_scopes = serde_json::Map::new();

  let base_import_map_dir = pop_last_segment(&base_import_map.base_url);

  if let Value::Object(base_value) = &mut base_import_map.import_map_value {
    if let Some(Value::Object(base_imports)) = base_value.remove("imports") {
      synth_import_map_imports = base_imports;
    }
    if let Some(Value::Object(base_scopes)) = base_value.remove("scopes") {
      synth_import_map_scopes = base_scopes;
    }
  }

  for child_config in children_import_maps.into_iter() {
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
    let Value::Object(mut import_map_obj) = child_config.import_map_value
    else {
      continue;
    };

    if let Some(Value::Object(imports)) = import_map_obj.remove("imports") {
      for (key, value) in imports {
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
        member_scope.insert(key, Value::String(value_relative_to_base_dir));
      }
    }
    combine_object(
      &mut synth_import_map_scopes,
      member_prefix,
      Value::Object(member_scope.clone()),
    );

    if let Some(Value::Object(scopes)) = import_map_obj.remove("scopes") {
      for (scope_name, scope_obj) in scopes {
        // Keys for scopes need to be processed - they might look like
        // "/foo/" and coming from "bar" workspace member. So we need to
        // prepend the member name to the scope.
        let Ok(scope_name_dir) = member_dir.join(&scope_name) else {
          combine_object(&mut synth_import_map_scopes, scope_name, scope_obj);
          continue; // not a file specifier
        };
        let Some(relative_to_base_dir) =
          base_import_map_dir.make_relative(&scope_name_dir)
        else {
          combine_object(&mut synth_import_map_scopes, scope_name, scope_obj);
          continue; // not a file specifier
        };
        let new_key = format!(
          "./{}/",
          relative_to_base_dir
            .strip_suffix('/')
            .unwrap_or(&relative_to_base_dir)
        );

        let mut new_scope = serde_json::Map::new();
        if let Value::Object(scope_obj) = scope_obj {
          for (key, value) in scope_obj {
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
            new_scope.insert(key, Value::String(value_relative_to_base_dir));
          }
        }
        combine_object(
          &mut synth_import_map_scopes,
          new_key,
          Value::Object(new_scope),
        );
      }
    }
  }

  let mut import_map = json!({});

  if !synth_import_map_imports.is_empty() {
    import_map["imports"] = Value::Object(synth_import_map_imports);
  }
  if !synth_import_map_scopes.is_empty() {
    import_map["scopes"] = Value::Object(synth_import_map_scopes);
  }

  (base_import_map.base_url, import_map)
}

fn combine_object(
  base: &mut serde_json::Map<String, Value>,
  property_name: String,
  addition: serde_json::Value,
) {
  if let Some(base_property) = base.get_mut(&property_name) {
    if let Some(base_property_obj) = base_property.as_object_mut() {
      if let Value::Object(addition) = addition {
        for (key, value) in addition.into_iter() {
          combine_object(base_property_obj, key, value);
        }
      }
    } else {
      base.insert(property_name, addition);
    }
  } else {
    base.insert(property_name, addition);
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
    let (url, value) = create_synthetic_import_map(base_import_map, children);
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
          },
          "./foo/": {
            "root": "./other.js",
            // this will be overwritten by the child
            "override": "./overwritten.js"
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
            "foo/": "./bar/",
            "override": "./overwritten.js"
          },
          "scopes": {
            "./fizz/": {
              "express": "npm:express@5",
            },
            "https://raw.githubusercontent.com/example/": {
              "https://example.com/": "https://deno.land/std@0.177.0/"
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
    let (url, value) = create_synthetic_import_map(base_import_map, children);
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
            "foo/": "./foo/bar/",
            "override": "./foo/overwritten.js",
            "root": "./other.js",
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
            "https://example.com/": "https://deno.land/std@0.177.0/",
          },
        },
      })
    );
  }

  #[test]
  fn test_expand_imports() {
    let import_map = json!({
      "imports": {
        "@std": "jsr:/@std",
        "@foo": "jsr:@foo",
        "express": "npm:express@4",
        "foo": "https://example.com/foo/bar"
      },
      "scopes": {}
    });
    let value = expand_import_map_value(import_map);
    assert_eq!(
      value,
      json!({
        "imports": {
          "@std": "jsr:/@std",
          "@std/": "jsr:/@std/",
          "@foo": "jsr:@foo",
          "@foo/": "jsr:/@foo/",
          "express": "npm:express@4",
          "express/": "npm:/express@4/",
          "foo": "https://example.com/foo/bar"
        },
        "scopes": {}
      })
    );
  }

  #[test]
  fn test_expand_imports_with_trailing_slash() {
    let import_map = json!({
      "imports": {
        "express": "npm:express@4",
        "express/": "npm:/express@4/foo/bar/",
      },
      "scopes": {}
    });
    let value = expand_import_map_value(import_map);
    assert_eq!(
      value,
      json!({
        "imports": {
          "express": "npm:express@4",
          "express/": "npm:/express@4/foo/bar/",
        },
        "scopes": {}
      })
    );
  }
}
