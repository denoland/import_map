// Copyright 2018-2025 the Deno authors. MIT license.

use thiserror::Error;
use url::Url;

#[derive(Debug, Error, Clone, PartialEq, Eq, deno_error::JsError)]
pub enum SpecifierError {
  // don't make this error a source because it's short
  // and that causes unnecessary verbosity
  #[class(inherit)]
  #[error("invalid URL: {0}")]
  InvalidUrl(url::ParseError),
  #[class(type)]
  #[error("Import \"{specifier}\" not a dependency")]
  ImportPrefixMissing {
    specifier: String,
    referrer: Option<Url>,
  },
}

/// Given a specifier string and a referring module specifier, try to resolve
/// the target module specifier, erroring if it cannot be resolved.
///
/// This function is useful for resolving specifiers in situations without an
/// import map.
pub fn resolve_import(
  specifier: &str,
  referrer: &Url,
) -> Result<Url, SpecifierError> {
  match Url::parse(specifier) {
    // 1. Apply the URL parser to specifier.
    //    If the result is not failure, return he result.
    Ok(url) => Ok(url),

    // 2. If specifier does not start with the character U+002F SOLIDUS (/),
    //    the two-character sequence U+002E FULL STOP, U+002F SOLIDUS (./),
    //    or the three-character sequence U+002E FULL STOP, U+002E FULL STOP,
    //    U+002F SOLIDUS (../), return failure.
    Err(url::ParseError::RelativeUrlWithoutBase)
      if !(specifier.starts_with('/')
        || specifier.starts_with("./")
        || specifier.starts_with("../")) =>
    {
      Err(SpecifierError::ImportPrefixMissing {
        specifier: specifier.to_string(),
        referrer: Some(referrer.clone()),
      })
    }

    // 3. Return the result of applying the URL parser to specifier with base
    //    URL as the base URL.
    Err(url::ParseError::RelativeUrlWithoutBase) => {
      referrer.join(specifier).map_err(SpecifierError::InvalidUrl)
    }

    // If parsing the specifier as a URL failed for a different reason than
    // it being relative, always return the original error. We don't want to
    // return `ImportPrefixMissing` or `InvalidBaseUrl` if the real
    // problem lies somewhere else.
    Err(err) => Err(SpecifierError::InvalidUrl(err)),
  }
}
