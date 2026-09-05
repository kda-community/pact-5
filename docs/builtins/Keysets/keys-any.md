## keys-any

Use `keys-any` as a keyset predicate function to determine if at least one of the keys defined in the keyset is matched.

### Basic syntax

To check whether one of the keys defined in a keyset is matched, use the following syntax:

```pact
(keys-any count matched)
```

**Note:** `keys-any` is intendeed to be used in keyset definitions, and not used directly in Pact code.

### Arguments

Use the following arguments to specify the count of keys in the keyset and the number of matched keys using the `keys-any` Pact function.

| Argument | Type | Description |
| --- | --- | --- |
| `count` | integer | Specifies the total count of keys defined in the keyset. |
| `matched` | integer | Specifies the number of matched keys. |

### Return value

The `keys-any` function returns a boolean value indicating whether one of the key in the keyset is matched.

### Examples

The following example demonstrates how to use the `keys-any` function to check whether one of the keys is matched in a keyset where the total number of keys defined is three:

```pact
pact> (keys-any 1 3)
true
```

The function returns true because one key in the keyset is matched.
