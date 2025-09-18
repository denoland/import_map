use std::cmp::Ordering;

use indexmap::map::Entry;
use thiserror::Error;
use url::Url;

use crate::{ImportMap, SpecifierMap};

pub(crate) fn code_unit_compare(a: &str, b: &str) -> Ordering {
  let mut a = a.encode_utf16();
  let mut b = b.encode_utf16();

  loop {
    match (a.next(), b.next()) {
      (Some(_), None) => return Ordering::Greater,
      (None, Some(_)) => return Ordering::Less,
      (Some(a), Some(b)) if a == b => {}
      (Some(a), Some(b)) => return a.cmp(&b),
      (None, None) => return Ordering::Equal,
    }
  }
}

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum MergeDiagnostic {
  #[error("Ignored specifier: {0} = {2:?}, using old value {1:?}")]
  ConflictingSpecifier(String, Option<Url>, Option<Url>),
}

fn merge_specifier_maps(
  old: &mut SpecifierMap,
  new: SpecifierMap,
  diag: &mut Vec<MergeDiagnostic>,
) {
  let old_max_index = old
    .inner
    .values()
    .map(|v| v.index)
    .max()
    .map(|index| index + 1)
    .unwrap_or(0);

  for (specifier, mut url) in new.inner {
    match old.inner.entry(specifier) {
      Entry::Occupied(v) => {
        if v.get().maybe_address != url.maybe_address {
          diag.push(MergeDiagnostic::ConflictingSpecifier(
            v.key().to_owned(),
            v.get().maybe_address.to_owned(),
            url.maybe_address,
          ))
        }
      }
      Entry::Vacant(v) => {
        // Place new entries after old entries
        url.index += old_max_index;
        v.insert(url);
      }
    }
  }
  old
    .inner
    .sort_by(|k1, _, k2, _| code_unit_compare(k1, k2).reverse());
}

pub(crate) fn merge(
  global: &mut ImportMap,
  new: ImportMap,
  diag: &mut Vec<MergeDiagnostic>,
) {
  for (scope_prefix, scope_imports) in new.scopes {
    // We don't have resolved module set at this level, in deno we assume we know all
    // the import maps ahead of time, skipping spec regarding resolved modules.

    match global.scopes.entry(scope_prefix) {
      Entry::Occupied(mut v) => {
        merge_specifier_maps(
          &mut v.get_mut().imports,
          scope_imports.imports,
          diag,
        );
      }
      Entry::Vacant(e) => {
        e.insert(scope_imports);
      }
    }
  }

  // Skipping integrity merging

  merge_specifier_maps(&mut global.imports, new.imports, diag);
}

#[cfg(test)]
mod tests {
  use url::Url;

  use crate::parse_from_json;

  use super::{code_unit_compare, merge, MergeDiagnostic};
  use std::cmp::Ordering;

  #[test]
  fn code_unit_spec_compare() {
    assert_eq!(code_unit_compare("～", "😀"), Ordering::Greater)
  }

  #[test]
  fn basic_merging() {
    let old = parse_from_json(
      Url::parse("file:///").unwrap(),
      r#"{
   "imports": {
    "/app/": "./original-app/"
  }
}"#,
    )
    .unwrap()
    .import_map;
    let new = parse_from_json(
      Url::parse("file:///").unwrap(),
      r#"{
  "imports": {
    "/app/helper": "./helper/index.mjs"
  },
  "scopes": {
    "/js": {
      "/app/": "./js-app/"
    }
  }
}"#,
    )
    .unwrap()
    .import_map;
    let mut result = old.clone();
    let mut diag = vec![];
    merge(&mut result, new, &mut diag);
    assert!(diag.is_empty());

    assert_eq!(
      result.to_json(),
      r#"{
  "imports": {
    "/app/": "./original-app/",
    "/app/helper": "./helper/index.mjs"
  },
  "scopes": {
    "/js": {
      "/app/": "./js-app/"
    }
  }
}
"#
    )
  }

  #[test]
  fn merge_with_conflicting_entry() {
    let old = parse_from_json(
      Url::parse("file:///").unwrap(),
      r#"{
   "imports": {
    "/app/helper": "./helper/index.mjs",
    "lodash": "/node_modules/lodash-es/lodash.js"
  }
}"#,
    )
    .unwrap()
    .import_map;
    let new = parse_from_json(
      Url::parse("file:///").unwrap(),
      r#"{
  "imports": {
    "/app/helper": "./main/helper/index.mjs"
  }
}"#,
    )
    .unwrap()
    .import_map;
    let mut result = old.clone();
    let mut diag = vec![];
    merge(&mut result, new, &mut diag);
    assert_eq!(
      diag,
      vec![MergeDiagnostic::ConflictingSpecifier(
        "file:///app/helper".to_owned(),
        Some(Url::parse("file:///helper/index.mjs").unwrap()),
        Some(Url::parse("file:///main/helper/index.mjs").unwrap())
      )]
    );

    assert_eq!(
      result.to_json(),
      r#"{
  "imports": {
    "/app/helper": "./helper/index.mjs",
    "lodash": "/node_modules/lodash-es/lodash.js"
  }
}
"#
    )
  }
}
