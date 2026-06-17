---
name: treemancer
description: Structural code search and rewrite using AST patterns via ast-grep. Use when text search is too brittle, you need syntax-aware queries, or want to find/transform code by structure rather than string content.
---

# Treemancer

## Quick Reference — Most Common First

### Pattern search
```bash
ast-grep run -p 'console.log($ARG)' -l ts .
ast-grep run -p 'async function $NAME($$$ARGS) { $$$BODY }' -l js src/
ast-grep run -p 'match $EXPR { $$$ARMS }' -l rs
ast-grep run -p 'pub fn $NAME($$$ARGS) -> $RET' -l rs
ast-grep run -p 'def $NAME' -l py
ast-grep run -p 'class { $$$ }' --selector field_definition -l js .
```

### Rewrite
```bash
ast-grep run -p 'oldFunction($ARG)' --rewrite 'newFunction($ARG)' -l ts .
```

### JSON output
```bash
ast-grep run -p 'console.log($ARG)' -l ts . --json | jq 'length'
ast-grep run -p 'function $NAME' -l js . --json | jq '[.[].metaVariables.single.NAME.text]'
```

### Rule file (complex / multi-condition)
```yaml
# rule.yml
id: async-no-trycatch
language: javascript
rule:
  all:
    - kind: function_declaration
    - has:
        pattern: await $EXPR
        stopBy: end
    - not:
        has:
          pattern: try { $$$ } catch ($E) { $$$ }
          stopBy: end
```
```bash
ast-grep scan --rule rule.yml src/
```

### Inline rule (no file)
```bash
ast-grep scan --inline-rules "id: my-rule
language: javascript
rule:
  kind: function_declaration
  has:
    pattern: await \$EXPR
    stopBy: end" .
```
**Escaping:** inside double quotes use `\$`; use single quotes or a rule file to avoid this entirely.

### Test against stdin (fast iteration)
```bash
echo "async function test() { await fetch(); }" | \
  ast-grep scan --inline-rules "id: t
language: js
rule:
  has:
    pattern: await \$EXPR
    stopBy: end" --stdin
```

---

## Workflow

**Pattern first, rule second.** Start with `ast-grep run -p` for single-node matches. Reach for a YAML rule when you need multiple conditions, relational logic (`inside`/`has`/`precedes`/`follows`), or composite logic (`all`/`any`/`not`).

**Rule file vs inline:**

| Use case | Approach |
|----------|----------|
| Quick one-off, ≤3 conditions | `--inline-rules` (no clean-up) |
| Complex / multi-condition rules | `.yml` rule file |
| Iterating on a tricky pattern | `.yml` file — edit and re-run |

**Debug when rules don't match:**
```bash
# Find correct kind: values — see how ast-grep parses target code
ast-grep run -p 'async function example() { await fetch(); }' -l js --debug-query=cst

# See how ast-grep interprets your pattern — debug metavariable capture
ast-grep run -p 'async function $NAME($$$ARGS) { $$$BODY }' -l js --debug-query=pattern
```

| Flag | Shows |
|------|-------|
| `--debug-query=cst` | All nodes including punctuation — find `kind:` names |
| `--debug-query=ast` | Named nodes only — clean tree view |
| `--debug-query=pattern` | How ast-grep parsed your pattern — verify metavariable placement |
| `--debug-query=sexp` | S-expression format — compact structural view |

**Iterate fast:** `echo "snippet" \| ast-grep scan --inline-rules "..." --stdin`. Test against a tiny snippet before running on the full codebase.

---

## Rule Reference

### Rule object anatomy

Every field within a rule object acts as **AND**. At least one positive key (`kind`, `pattern`, etc.) must be present.

```yaml
rule:
  kind: call_expression       # node type (find values with --debug-query=cst)
  pattern: foo($ARG)          # code pattern (must be syntactically complete)
  regex: ^[a-z]+$             # Rust regex against node text
  nthChild: 1                 # position in parent (1-based)
  range:                      # character positions (start inclusive, end exclusive)
    start: { line: 0, column: 0 }
    end:   { line: 0, column: 10 }
  inside: { ... }             # must be inside matching ancestor
  has: { ... }                # must have matching descendant
  precedes: { ... }           # appears before matching sibling
  follows: { ... }            # appears after matching sibling
  all: [ ... ]                # AND — guarantees left-to-right evaluation order
  any: [ ... ]                # OR
  not: { ... }                # NOT
```

### Atomic rules

**`pattern`** — Match by code pattern. Metavariables (`$ARG`, `$$$ARGS`) stand in for dynamic parts.

String form (most common):
```yaml
pattern: console.log($ARG)
```

Object form (ambigous patterns / sub-node targeting):
```yaml
pattern:
  context: class { $F }       # surrounding code for correct parsing
  selector: field_definition   # pinpoint a sub-node within the parsed pattern
  strictness: relaxed          # cst | smart | ast | relaxed | signature
```
Use `context` + `selector` when a pattern fragment can't parse standalone (e.g. a class `field_definition` outside a class body). `--selector` on the CLI is the equivalent of `pattern.selector`.

**`kind`** — Match by tree-sitter AST node kind. Find correct values with `--debug-query=cst`.
```yaml
kind: call_expression
kind: function_declaration
kind: method_definition
```

**`regex`** — Match the entire text of a node against a Rust regex.
```yaml
regex: ^[a-z_]+$
```

**`nthChild`** — Match by position in parent's named children. Simple number, An+B formula, or object form with `reverse`/`ofRule`.
```yaml
nthChild: 1              # first child
nthChild: 2n+1           # An+B formula (odd: 1st, 3rd, 5th...)
nthChild:
  position: 2n+1
  reverse: true          # count from end
  ofRule: { kind: statement }
```

**`range`** — Match by character start/end positions.
```yaml
range:
  start: { line: 0, column: 4 }
  end:   { line: 0, column: 19 }
```

### Relational rules

**Always use `stopBy: end`** on `inside` and `has` — the default `neighbor` stops at the first non-matching boundary and is almost never what you want.

`stopBy` options:
- `neighbor` (default) — stops at immediate non-matching boundary
- `end` — searches to root (`inside`) or farthest leaf (`has`)
- `{ Rule }` — stops when a matching node is encountered

**`field`** (only `inside` / `has`) — restricts the match to a specific named child field. Use this for operator matching.

**`inside`** — Target must be inside a node matching the sub-rule.
```yaml
inside:
  pattern: class $C { $$$ }
  stopBy: end
  field: body            # optional: restrict to named child
```

**`has`** — Target must have a descendant matching the sub-rule.
```yaml
has:
  pattern: await $EXPR
  stopBy: end
  field: body            # optional
```

**`precedes` / `follows`** — Sequential ordering within the nearest shared parent (source order).
```yaml
precedes: { pattern: return $VAL, stopBy: end }
follows:  { pattern: import $M }
```
`stopBy` is available; `field` is not.

**Operator matching with `field` + `$$OP`:**
```yaml
has:
  field: operator
  pattern: $$OP
  stopBy: end
```
This captures `+`, `*`, `->`, `===`, etc.

### Composite rules

| Rule | Meaning | Example |
|------|---------|---------|
| `all` | All sub-rules must match (AND) | `all: [ { kind: call_expression }, { pattern: foo($A) } ]` |
| `any` | Any sub-rule must match (OR) | `any: [ { pattern: foo() }, { pattern: bar() } ]` |
| `not` | Sub-rule must NOT match | `not: { pattern: console.log($ARG) }` |

`all` guarantees left-to-right evaluation — important when later sub-rules depend on metavariables from earlier ones.

---

## Metavariables

| Syntax | Captures | Notes |
|--------|----------|-------|
| `$NAME` | One named AST node | Reusable: `$A == $A` requires both sides identical |
| `$$OP` | One unnamed node (operators, punctuation) | Use with `field: operator` to capture `+`, `*`, `===` |
| `$$$ARGS` | Zero or more nodes (non-greedy) | `console.log($$$)` matches 0-N args |
| `$_` / `$_IGNORED` | Non-capturing wildcard | Optimized — no back-reference, matches anything freely |

**Metavariables must be the sole content of their AST node.** `obj.on$EVENT`, `"Hello $WORLD"`, `a $OP b` are invalid.

---

## Common Patterns

### Function containing `await`
```yaml
rule:
  kind: function_declaration
  has:
    pattern: await $EXPR
    stopBy: end
```

### Calls inside a method
```yaml
rule:
  pattern: console.log($$$)
  inside:
    kind: method_definition
    stopBy: end
```

### Async function missing try-catch
```yaml
rule:
  all:
    - kind: function_declaration
    - has: { pattern: await $EXPR, stopBy: end }
    - not:
        has: { pattern: try { $$$ } catch ($E) { $$$ }, stopBy: end }
```

### Multiple alternatives
```yaml
rule:
  any:
    - pattern: console.log($$$)
    - pattern: console.warn($$$)
    - pattern: console.error($$$)
```

### Pattern inside a specific class method
```yaml
rule:
  pattern: this.$METHOD($$$)
  inside:
    pattern: class $C { $$$ }
    stopBy: end
```

---

## Troubleshooting

1. **No matches?** Simplify the rule (remove sub-rules). Use `--debug-query=cst` to verify `kind:` values against the actual AST.
2. **Relational rules not working?** Add `stopBy: end`. The default `neighbor` is almost always too restrictive.
3. **Metavariable not capturing?** Ensure it's the sole content of its AST node. Use `--debug-query=pattern` to see how ast-grep parsed it.
4. **Pattern too complex?** Break into simpler sub-rules with `all`, or switch to a `.yml` file instead of inline.
