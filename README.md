# typecast

A Common Lisp CLI tool that reads a JSON schema and generates Rust source files for [ratatui](https://github.com/ratatui-org/ratatui) TUI form components.

Given a schema describing your configuration types, `typecast` emits:
- Rust `struct`/`enum` data types with `serde` derives
- `FormComponent` implementations for interactive editing in a terminal UI
- Shared primitive components (`TextInput`, `NumericInput`, `Toggle`, `SelectList`, etc.)
- A `RootForm` wiring everything together
- A `mod.rs` with all declarations and re-exports

## Requirements

- [SBCL](https://www.sbcl.org/)
- [Quicklisp](https://www.quicklisp.org/) (for `yason` and `alexandria`)

## Build

```bash
make
```

Produces a standalone `./typecast` binary. To rebuild after source changes, just run `make` again.

```bash
make clean   # remove binary and test-output/
```

## Usage

```bash
./typecast schema.json src/generated/
```

Two arguments: schema file, output directory. The output directory is created if it does not exist. All generated files are overwritten on each run — do not edit them by hand.

The output directory should be a dedicated subdirectory inside your Rust project, not the project root. It acts as a Rust module:

```
my-rust-app/
  src/
    main.rs
    generated/       ← output-dir points here
      mod.rs
      traits.rs
      app_config.rs
      ...
```

Then declare the module in `src/main.rs` (or `lib.rs`):

```rust
mod generated;
```

## Writing your schema

Create a JSON file with this shape:

```json
{
  "version": 1,
  "root": "MyConfig",
  "types": { ... }
}
```

- `root` — the name of the top-level type; this becomes the `RootForm` component
- `types` — a map of type names to type definitions

### Defining a struct (object)

```json
"ServerConfig": {
  "kind": "object",
  "fields": [
    { "key": "host",    "type": "string", "required": true,  "label": "Host" },
    { "key": "port",    "type": "int",    "required": true,  "label": "Port",
      "constraints": { "min": 1, "max": 65535 } },
    { "key": "timeout", "type": "float",  "required": false, "label": "Timeout (s)" }
  ]
}
```

Optional fields (`"required": false`) become `Option<T>` in Rust and show a toggle in the TUI to enable/disable them.

### Defining an enum

```json
"LogLevel": {
  "kind": "enum",
  "default": "info",
  "values": [
    { "value": "debug", "label": "Debug" },
    { "value": "info",  "label": "Info" },
    { "value": "warn",  "label": "Warn" },
    { "value": "error", "label": "Error" }
  ]
}
```

Enums render as a scrollable `SelectList` in the TUI. `default` must match one of the `value` strings.

### Defining a tagged union

```json
"AuthMethod": {
  "kind": "tagged_union",
  "discriminator": "method",
  "default_variant": "password",
  "variants": [
    {
      "value": "password",
      "label": "Password",
      "fields": [
        { "key": "username", "type": "string", "required": true, "label": "Username" },
        { "key": "password", "type": "string", "required": true, "label": "Password" }
      ]
    },
    {
      "value": "token",
      "label": "API Token",
      "fields": [
        { "key": "token", "type": "string", "required": true, "label": "Token" }
      ]
    },
    {
      "value": "none",
      "label": "No auth",
      "fields": []
    }
  ]
}
```

The TUI shows a variant selector; switching variant swaps the visible fields below it. `discriminator` becomes the serde tag key.

### Referencing other types

Use the type name as the type expression for a field:

```json
{ "key": "auth", "type": "AuthMethod", "required": true, "label": "Auth" }
```

For lists and maps:

```json
{ "key": "tags",    "type": "[string]",          "required": false, "label": "Tags" },
{ "key": "headers", "type": "map<string,string>", "required": false, "label": "Headers" }
```

Types can reference other types freely; `typecast` resolves the dependency order automatically.

## Type reference

### Kinds

| Kind | Description |
|------|-------------|
| `object` | Rust struct with named fields |
| `enum` | Simple string enum rendered as a `SelectList` |
| `tagged_union` | Tagged union (sum type) with per-variant fields; rendered as a variant switcher + nested form |

### Type expressions

| Expression | Rust type |
|------------|-----------|
| `"string"` | `String` |
| `"int"` | `i64` |
| `"float"` | `f64` |
| `"bool"` | `bool` |
| `"any"` | `serde_json::Value` |
| `"TypeName"` | named type from your schema |
| `"[TypeName]"` | `Vec<TypeName>` |
| `"map<string,TypeName>"` | `HashMap<String, TypeName>` |

### Field properties

| Property | Required | Description |
|----------|----------|-------------|
| `key` | yes | camelCase name; becomes the JSON key and (as snake_case) the Rust field name |
| `type` | yes | Type expression (see above) |
| `required` | yes | `true` = always present; `false` = `Option<T>` |
| `label` | no | Display name shown in the TUI |
| `help` | no | Help text shown below the field |
| `placeholder` | no | Placeholder text for text inputs |
| `default` | no | Default value pre-filled in the form |
| `constraints` | no | Validation limits (see below) |

### Constraints

```json
"constraints": { "min": 0, "max": 100 }
```

| Key | Applies to | Description |
|-----|-----------|-------------|
| `min` | `int`, `float` | Minimum value |
| `max` | `int`, `float` | Maximum value |
| `min_items` | lists | Minimum number of items |
| `max_items` | lists | Maximum number of items |

## Generated output structure

```
output-dir/
  mod.rs              # pub mod declarations + re-exports
  traits.rs           # FormComponent trait, EventResult enum
  primitives.rs       # TextInput, NumericInput, FloatInput, Toggle, SelectList, JsonValueEditor
  list_editor.rs      # Vec<T> editor component
  kv_editor.rs        # HashMap<String, V> editor component
  optional_field.rs   # Option<T> wrapper component
  <type_name>.rs      # one file per schema type (data struct + FormComponent impl)
  root_form.rs        # RootForm wiring the root type's component
```

All files are prefixed with `// Generated by typecast — do not edit by hand`.

## Error messages

If your schema has a problem, `typecast` exits with a non-zero status and prints what went wrong:

```
Error: Unresolved named references:
  ServerConfig: unresolved reference to TlsOptions
```

```
Error: Cycle detected among types: A, B
```

Fix the schema and re-run.

## Integrating into a build

Add a step to regenerate before `cargo build`:

```bash
./typecast config-schema.json src/generated/ && cargo build
```

Or in a `Makefile`:

```makefile
src/generated/mod.rs: config-schema.json
	./typecast config-schema.json src/generated/

build: src/generated/mod.rs
	cargo build
```

The generated files are ordinary Rust source — commit them alongside your schema, or regenerate them in CI, whichever fits your workflow.

## How it works

1. **Parse** — reads the JSON schema into an AST (`schema`, `type-def`, `field-def`, `type-ref` structs)
2. **Validate** — checks all named references resolve; errors list every broken reference
3. **Topological sort** — orders types so dependencies are emitted before the types that use them
4. **Render & emit** — produces Rust source strings and writes them to disk
