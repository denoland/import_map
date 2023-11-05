// Copyright 2021-2023 the Deno authors. All rights reserved. MIT license.

import {
  assertEquals,
  assertThrows,
} from "https://deno.land/std@0.205.0/assert/mod.ts";
import { parseFromJson } from "./mod.ts";

Deno.test({
  name: "parseFromJson() - object import map",
  async fn() {
    const importMap = await parseFromJson(
      "file:///a/import-map.json",
      {
        imports: {
          "jsdom": "https://esm.sh/jsdom",
        },
      },
    );
    assertEquals(
      importMap.resolve("jsdom", "https://deno.land/x/oak/mod.ts"),
      "https://esm.sh/jsdom",
    );
    assertThrows(
      () => {
        importMap.resolve("notfound", "https://deno.land/x/oak/mod.ts");
      },
      Error,
      "Relative import path",
    );
  },
});

Deno.test({
  name: "parseFromJson() - JSON string",
  async fn() {
    const importMap = await parseFromJson(
      "file:///a/import-map.json",
      JSON.stringify({
        imports: {
          "jsdom": "https://esm.sh/jsdom",
        },
      }),
    );
    assertEquals(
      importMap.resolve("jsdom", "https://deno.land/x/oak/mod.ts"),
      "https://esm.sh/jsdom",
    );
    assertThrows(
      () => {
        importMap.resolve("notfound", "https://deno.land/x/oak/mod.ts");
      },
      Error,
      "Relative import path",
    );
  },
});
