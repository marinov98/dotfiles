# Coding Philosophy

Guidelines, not dogma. Follow the project's existing conventions when they conflict.

- **Think in types first.** Design types before code; make illegal states unrepresentable.
- **Parse, don't validate.** Turn unstructured input into precise types once at the boundary.
- **Pure core, impure shell.** Keep business logic pure; push IO/network/DB to the outermost layer.
- **Errors as values.** Model expected failures in return types; reserve exceptions for unrecoverable cases.
- **Separate data from behavior.** Plain data structures; behavior in functions. Composition over inheritance.
- **Design deep modules.** Simple interfaces, powerful implementations. Pull complexity downward.

# CLI Tooling

## Why These Tools

These tools respect `.gitignore` by default, filtering `.git/`, `.venv`, `node_modules/`, and build artifacts. With a venv present, `find` returns **937x more files** and `grep` returns **310x more results** — every extra result is wasted context tokens.

| Tool            | Purpose                   | Key Advantage                          |
| --------------- | ------------------------- | -------------------------------------- |
| `rg`            | Content search            | Gitignore-aware; 98x faster with venvs |
| `fd`            | File discovery            | Gitignore-aware; 937x fewer results    |
| `sd`            | Text replacement          | Cleaner syntax than sed; `-F` literal  |
| `scc`           | Codebase overview         | LOC, complexity, language breakdown    |
| `ast-grep (sg)` | Structural code transform | AST patterns + `--rewrite`             |
| `jq`            | JSON processing           | Pipe from scc, git, package managers   |
| `shellcheck`    | Shell validation          | Catches bugs before they ship          |
| `yq`            | YAML/INI processing       | Query/edit with comment preservation   |

**Not used:** `grep`/`find` (no gitignore), `sed` (sd is cleaner), `bat` (redundant with `read`)

## Search Strategy

1. `rg -l "pattern"` — locate files (gitignore-aware, fastest)
2. `fd -e ext . path/` — scope workspace by extension
3. `rg -n -C 2 "pattern"` — read context around matches
4. `sg -p 'pattern' -l lang` — structural search for code patterns
5. `scc` — understand codebase composition
6. Built-in `read` — deep dive into specific files

## Pipelines

- `fd -e py | xargs rg "pat"` — scope text search to file types
- `scc --format json | jq '.[] | {lang: .Name, files: .Count}'` — structured queries
- `sd 'old' 'new' file` — simple replacement (no escaping needed)
- `sd -F '$var' 'val' file` — literal replacement (metacharacters safe)
- `ast-grep -p 'pat' --rewrite 'new' -l lang` — structural code transformation

## ast-grep (sg) — Syntax Reminder

Patterns must be **syntactically complete code**:

```
ast-grep -p 'def $NAME' -l py              # Python functions
ast-grep -p 'pub fn $NAME' -l rust         # Rust public functions
ast-grep -p 'match $EXPR { $$$ARMS }' -l rust  # Match expressions
ast-grep -p 'X' --rewrite 'Y' -l lang     # Structural rename
```

- `$NAME` = any identifier
- `$$$ARGS` = zero-or-more (in function args, match arms, etc.)
- `$_NAME` = non-capturing match (performance optimization)
- `--dry-run` not supported — test with `ast-grep -p 'pat'` first, then add `--rewrite`
- Patterns that fail: incomplete statements (`def main()`, `fn main()`), multi-var in certain positions (`fn $NAME($$$ARGS)`)

# Workflow

- **Plan first.** For 3+ step tasks, plan before coding. If sideways, STOP and re-plan.
- **Verify before done.** Run tests, check logs, demonstrate correctness.
- **Fix bugs autonomously.** Find root cause; no temporary patches.
- **Demand elegance, in balance.** Pause for "is there a cleaner way?" on non-trivial changes.
- **Keep changes small.** Touch only what's necessary.
