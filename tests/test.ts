// Copyright 2021-2023 the Deno authors. All rights reserved. MIT license.

import {
  assertEquals,
  assertThrows,
} from "https://deno.land/std@0.205.0/assert/mod.ts";
import { parseFromJson } from "../js/mod.ts";

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

Deno.test({
  name: "ImportMap - resolve() - jsr",
  async fn() {
    const importMap = await parseFromJson(
      "file:///a/import-map.json",
      {
        imports: {
          "@std/assert": "jsr:@std/assert",
          "@std/testing": "jsr:@std/testing",
        },
      },
    );
    assertEquals(
      importMap.resolve("@std/assert", "https://deno.land/x/oak/mod.ts"),
      "jsr:@std/assert",
    );
    assertEquals(
      importMap.resolve("@std/testing/bdd", "https://deno.land/x/oak/mod.ts"),
      "jsr:/@std/testing/bdd",
    );
  },
});
