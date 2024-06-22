// Copyright 2018-2024 the Deno authors. MIT license.

use std::error::Error;
use std::fmt;

use url::Url;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpecifierError {
  InvalidUrl(url::ParseError),
  ImportPrefixMissing {
    specifier: String,
    referrer: Option<Url>,
  },
}

impl Error for SpecifierError {
  fn source(&self) -> Option<&(dyn Error + 'static)> {
    match self {
      Self::InvalidUrl(ref err) => Some(err),
      _ => None,
    }
  }
}

impl fmt::Display for SpecifierError {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Self::InvalidUrl(ref err) => write!(f, "invalid URL: {err}"),
      Self::ImportPrefixMissing { specifier, .. } => write!(
        f,
        "Relative import path \"{specifier}\" not prefixed with / or ./ or ../",
      ),
    }
  }
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