# Coding Philosophy

Guidelines, not dogma. Follow the project's existing conventions when they conflict.

- **Think in types first.** The type signatures matter more than the implementation.
  Design the types and signatures before the code itself, and make illegal states unrepresentable.
- **Parse, don't validate.** Turn unstructured input into precise types once, at the boundary,
  so the rest of the code can trust it.
- **Pure core, impure shell.** Keep business logic pure and deterministic; push side effects
  (IO, network, DB, clock, randomness) to the outermost layer.
- **Minimize side effects.** Prefer immutability and expressions over mutation and statements.
- **Errors as values.** Model expected failures in the return type rather than throwing;
  reserve exceptions for truly unrecoverable situations
- **Separate data from behavior.** Keep data structures plain and transparent;
  put behavior in functions that operate on that data. Prefer **composition over inheritance**.
- **Prefer dependency injection.** Pass dependencies in explicitly rather than reaching out to construct or locate them.
- **Design deep modules.** Prefer simple interfaces over powerful implementations;
  pull complexity downward so callers stay simple. Where possible, design errors out of existence rather than handling them.

# CLI Tooling

## Why These Tools

These tools respect `.gitignore` by default, filtering `.git/`, `.venv`, `node_modules/`, and build artifacts.
With a venv present, `find` (can) return **937x more files** and `grep` (can) return **310x more results** — every extra result is wasted context tokens.

| Tool            | Purpose                   | Key Advantage                          |
| --------------- | ------------------------- | -------------------------------------- |
| `rg (ripgrep)`  | Content search            | Gitignore-aware; 98x faster with venvs |
| `fd`            | File discovery            | Gitignore-aware; 937x fewer results    |
| `sd`            | Text replacement          | Cleaner syntax than sed; `-F` literal  |
| `scc`           | Codebase overview         | LOC, complexity, language breakdown    |
| `ast-grep (sg)` | Structural code transform | AST patterns + `--rewrite`             |
| `jq`            | JSON processing           | Pipe from scc, git, package managers   |
| `yq`            | YAML/INI/XML processing   | Query/edit with comment preservation   |
| `shellcheck`    | Shell validation          | Catches bugs before they ship          |

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
ast-grep -p 'X' --rewrite 'Y' -l rust     # Structural rename
```

- `$NAME` = any identifier
- `$$$ARGS` = zero-or-more (in function args, match arms, etc.)
- `$_NAME` = non-capturing match (performance optimization)
- `--dry-run` not supported — test with `ast-grep -p 'pat'` first, then add `--rewrite`
- Patterns that fail: incomplete statements (`def main()`, `fn main()`), multi-var in certain positions (`fn $NAME($$$ARGS)`)

# Workflow

- **Plan first.** For any non-trivial task (3+ steps or an architectural decision), plan
  the approach before writing significant code. If something goes sideways, STOP and re-plan
  rather than pushing forward.
- **Verify before done.** Never mark a task complete without proving it works, run tests, check logs, and
  demonstrate correctness. Ask: "Would a staff engineer approve this?"
- **Fix bugs autonomously.** Given a bug report, failing test, or error: just fix it. Find the root cause;
  no temporary patches or hand-holding.
- **Demand elegance, in balance.** For non-trivial changes, pause and ask "is there a cleaner way?"
  before presenting. Skip this for simple, obvious fixes — don't over-engineer.
- **Keep changes small and focused.** Touch only what's necessary; avoid bundling
  unrelated refactors.
