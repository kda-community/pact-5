## error

Use `error` to trigger unconditionnaly a transaction failure.

Roughly equialent to ``(enforce false message)``.

### Basic syntax

To fail a transaction with a specified error message use the following syntax:

```pact
(error message)
```

### Arguments

Use the following arguments to specify the test expression and error message for the `error` Pact function:

| Argument  | Type   | Description                             |
|-----------|--------|-----------------------------------------|
| `message` | string | Specifies the error message to display. |

### Return value

The function never returns.

### Examples

The following example demonstrates how to use the `error` function.

```pact
pact> (error "We are always wrong")
(interactive):1:0: We are always wrong
```

The folowing exemple defines the following function using ``cond`` and ``error``:

```
y
2 |●
  | \
  |  \
1 |   ●────────●
  |             \
  |              \
0 |               ●
  +----------------→ x
    0   1   2   3
```

```pact
(defun my-function: decimal (x:decimal)
  (cond
    ((> x 3.0) 0.0)
    ((> x 2.0) (- 3.0 x))
    ((> x 1.0) 1.0)
    ((> x 0.0) (- 2.0 x))
    (error "Function undefined for negative values")
  )
)

pact> (my-function 2.1)
0.9
pact> (my-function -3.0)
abcd.repl:7:4: Function undefined for negative values
```