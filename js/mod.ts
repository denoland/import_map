// Copyright 2021-2023 the Deno authors. All rights reserved. MIT license.

/**
 * Provides a [spec compliant](https://wicg.github.io/import-maps/)
 * implementation of import maps, using the same code the Deno CLI uses when
 * handling import maps.
 *
 * @module
 */
import { instantiate, JsImportMap } from "./import_map.generated.js";

let wasm: Awaited<ReturnType<typeof instantiate>> | undefined;

export class ImportMap {
  #inner: JsImportMap;

  constructor(inner: JsImportMap) {
    this.#inner = inner;
  }

  /** Given a specifier and a referrer, return the resolved string URL. */
  resolve(specifier: string, referrer: string | URL) {
    if (referrer instanceof URL) {
      referrer = referrer.toString();
    }
    return this.#inner.resolve(specifier, referrer);
  }

  /** Return the value of the import map as a JSON string. */
  toJSON(): string {
    return this.#inner.toJSON();
  }
}

export interface ImportMapJson {
  imports: Record<string, string>;
  scopes?: Record<string, Record<string, string>>;
}

/** Given a base and a JSON string with the contents of a import map,  */
export async function parseFromJson(
  baseUrl: string | URL,
  json: string | ImportMapJson,
): Promise<ImportMap> {
  wasm = wasm ?? await instantiate();
  if (baseUrl instanceof URL) {
    baseUrl = baseUrl.toString();
  }
  if (typeof json === "object") {
    json = JSON.stringify(json);
  }
  const inner = wasm.parseFromJson(baseUrl, json);
  return new ImportMap(inner);
}
