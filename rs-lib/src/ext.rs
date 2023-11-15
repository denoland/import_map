// Copyright 2021-2022 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;

use serde_json::json;
use serde_json::Value;
use url::Url;

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
  let mut synth_import_map_imports = json!({});
  let mut synth_import_map_scopes = json!({});
  let synth_import_map_imports_obj =
    synth_import_map_imports.as_object_mut().unwrap();
  let synth_import_map_scopes_obj =
    synth_import_map_scopes.as_object_mut().unwrap();

  let base_import_map_dir = pop_last_segment(&base_import_map.base_url);

  for child_config in children_import_maps.iter() {
    let mut member_scope = json!({});
    let member_scope_obj = member_scope.as_object_mut().unwrap();

    let member_dir = pop_last_segment(&child_config.base_url);
    let relative = base_import_map_dir.make_relative(&member_dir).unwrap();
    let member_prefix =
      format!("./{}/", relative.strip_suffix('/').unwrap_or(&relative));

    if let Some(imports) = child_config.import_map_value.get("imports") {
      for (key, value) in imports.as_object().unwrap() {
        let Some(value_str) = value.as_str() else {
          continue;
        };
        let value_url = member_dir.join(value_str).unwrap();
        // `make_relative` can fail here if the provided value is already
        // a fully resolved URL.
        let value_relative = match base_import_map_dir.make_relative(&value_url)
        {
          Some(v) => format!("./{}", v),
          None => value_str.to_string(),
        };
        member_scope_obj.insert(key.to_string(), Value::String(value_relative));
      }
    }
    synth_import_map_scopes_obj.insert(member_prefix.to_string(), member_scope);

    if let Some(scopes) = child_config.import_map_value.get("scopes") {
      for (scope_name, scope_obj) in scopes.as_object().unwrap() {
        // Keys for scopes need to be processed - they might look like
        // "/foo/" and coming from "bar" workspace member. So we need to
        // prepend the member name to the scope.
        let scope_name_dir = member_dir.join(scope_name).unwrap();
        let relative =
          base_import_map_dir.make_relative(&scope_name_dir).unwrap();
        let new_key =
          format!("./{}/", relative.strip_suffix('/').unwrap_or(&relative));

        let mut new_scope: HashMap<String, String> = HashMap::default();
        for (key, value) in scope_obj.as_object().unwrap() {
          let Some(value_str) = value.as_str() else {
            continue;
          };
          let value_url = member_dir.join(value_str).unwrap();
          // `make_relative` can fail here if the provided value is already
          // a fully resolved URL.
          let value_relative =
            match base_import_map_dir.make_relative(&value_url) {
              Some(v) => format!("./{}", v),
              None => value_str.to_string(),
            };
          new_scope.insert(key.to_string(), value_relative);
        }
        synth_import_map_scopes_obj
          .insert(new_key, serde_json::to_value(new_scope).unwrap());
      }
    }
  }

  if let Some(base_imports) = base_import_map.import_map_value.get("imports") {
    let base_imports_obj = base_imports.as_object().unwrap();
    for (key, value) in base_imports_obj.iter() {
      synth_import_map_imports_obj.insert(key.to_owned(), value.to_owned());
    }
  }
  if let Some(base_scopes) = base_import_map.import_map_value.get("scopes") {
    let base_scopes_obj = base_scopes.as_object().unwrap();
    for (key, value) in base_scopes_obj.iter() {
      synth_import_map_scopes_obj.insert(key.to_owned(), value.to_owned());
    }
  }

  let mut import_map = json!({});

  if !synth_import_map_imports_obj.is_empty() {
    import_map["imports"] = synth_import_map_imports;
  }
  if !synth_import_map_scopes_obj.is_empty() {
    import_map["scopes"] = synth_import_map_scopes;
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
        "scopes": {}
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
          }
        },
      })
    );
  }
}
