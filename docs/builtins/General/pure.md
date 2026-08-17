## pure

Use the `pure` function to run a **pure** (read-only) action.

If the action is not pure and attempts to modify the blockchain state, a failure is triggered.

This function is intended to be used to wrap untrusted code (especially modref calls) to prevent potential side effects.

### Basic syntax

To execute a pure action and return its result, use the following syntax:

```pact
(pure action)
```

### Arguments

Use the following argument to specify the action to be executed by the `pure` Pact function.

| Argument | Type | Description            |
|----------|------|------------------------|
| `action` | any  | The action to execute. |

### Return value

The `pure` function returns the result of the action.

### Examples

The following example demonstrates how to use the `pure` function in the Pact REPL.

This example wraps a simple pure expression.

```pact
(pure (+ 1 2))
3
```

The following example shows a `pure`-wrapped write failure and a `pure`-wrapped read success.

```pact
pact> (module reader-writer G
  (defcap G() true)

  (defschema my-schema
    a:integer)

  (deftable my-table:{my-schema})

  (defun do-read() (read my-table ""))

  (defun do-write() (write my-table "" {'a:1}))
)
(create-table my-table)
(do-write)

pact> (pure (reader-writer.do-write))
test_module.pact:11:20: Error during database operation: Operation disallowed in read-only or sys-only mode

pact> (pure (reader-writer.do-read))
{"a": 1}
```