// Copyright 2021-2022 the Deno authors. All rights reserved. MIT license.

use serde_json::json;
use serde_json::Value;
use url::Url;

pub struct ImportMapConfig {
  base_url: Url,
  scope_prefix: String,
  imports: Value,
  scopes: Value,
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

  for child_config in children_import_maps.iter() {
    let mut member_scope = json!({});

    let member_scope_obj = member_scope.as_object_mut().unwrap();
    for (key, value) in child_config.imports.as_object().unwrap() {
      member_scope_obj.insert(key.to_string(), value.to_owned());
    }
    // TODO(bartlomieju): this need to resolve values in member_scope based
    // on the "base URL" of the member import map filepath
    synth_import_map_scopes_obj
      .insert(format!("./{}/", child_config.scope_prefix), member_scope);

    for (key, value) in child_config.scopes.as_object().unwrap() {
      // Keys for scopes need to be processed - they might look like
      // "/foo/" and coming from "bar" workspace member. So we need to
      // prepend the member name to the scope.
      let new_key = format!("./{}{}", child_config.scope_prefix, key);
      // TODO(bartlomieju): this need to resolve value based on the "base URL"
      // of the member import map filepath
      synth_import_map_scopes_obj.insert(new_key, value.to_owned());
    }
  }

  let base_imports_obj = base_import_map.imports.as_object().unwrap();
  for (key, value) in base_imports_obj.iter() {
    synth_import_map_imports_obj.insert(key.to_owned(), value.to_owned());
  }
  let base_scopes_obj = base_import_map.scopes.as_object().unwrap();
  for (key, value) in base_scopes_obj.iter() {
    synth_import_map_scopes_obj.insert(key.to_owned(), value.to_owned());
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
