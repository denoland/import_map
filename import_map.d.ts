// @generated file from wasmbuild -- do not edit
// deno-lint-ignore-file
// deno-fmt-ignore-file

export class JsImportMap {
  private constructor();
  free(): void;
  [Symbol.dispose](): void;
  resolve(specifier: string, referrer: string): string;
  toJSON(): string;
}

export function parseFromJson(
  base_url: string,
  json_string: string,
  expand_imports: boolean,
): JsImportMap;
