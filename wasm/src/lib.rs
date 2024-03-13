// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use import_map::{parse_from_json_with_options, ImportMap, ImportMapOptions};
use url::Url;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct JsImportMap(ImportMap);

#[wasm_bindgen]
impl JsImportMap {
  #[wasm_bindgen]
  pub fn resolve(
    &self,
    specifier: String,
    referrer: String,
  ) -> Result<String, JsError> {
    let referrer =
      Url::parse(&referrer).map_err(|err| JsError::new(&err.to_string()))?;
    self
      .0
      .resolve(&specifier, &referrer)
      .map(|url| url.to_string())
      .map_err(|err| JsError::new(&err.to_string()))
  }

  #[wasm_bindgen(js_name = toJSON)]
  pub fn to_json(&self) -> String {
    self.0.to_json()
  }
}

#[wasm_bindgen(js_name = parseFromJson)]
pub fn js_parse_from_json(
  base_url: String,
  json_string: String,
) -> Result<JsImportMap, JsError> {
  let base_url =
    Url::parse(&base_url).map_err(|err| JsError::new(&err.to_string()))?;
  parse_from_json_with_options(
    &base_url,
    &json_string,
    ImportMapOptions {
      address_hook: None,
      expand_imports: true,
    },
  )
  .map(|map_with_diag| JsImportMap(map_with_diag.import_map))
  .map_err(|err| JsError::new(&err.to_string()))
}
