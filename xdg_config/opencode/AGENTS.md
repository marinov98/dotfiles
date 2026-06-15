# Coding Philosophy

These are guidelines, not dogma. Prefer them where possible, but follow the project's existing
conventions when they conflict. Apply the spirit of these even in languages where the exact mechanism
(e.g. static types) isn't available.

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

# CLI Tool Preferences

Prefer these tools via `bash` over POSIX alternatives. ALWAYS use the best available tool. NEVER
fall back to a weaker alternative when a better one is installed.

## File & Content Search

- **`rg`** (ripgrep) over `grep` — respects `.gitignore`, faster
- **`fd`** over `find` — respects `.gitignore`, sane defaults
- **`ast-grep`** (`sg` alias) for AST-aware structural code search/refactor — use when regex is fragile (e.g., "find all calls to X with 3+ args", "rewrite optional chaining")

## Text Manipulation

- **`sd`** over `sed` — PCRE regex, use `-F` for literal strings with metacharacters
- **`ast-grep --pattern '<PATTERN>' --rewrite '<TEMPLATE>'`** for structural refactoring

## Validation & Analysis

- **`shellcheck`** — validate every `.sh` script I generate
- **`scc`** — codebase overview (languages, LOC, complexity) before diving in
- **`yq`** — query/edit YAML (preserves comments/formatting)
- **`jq`** — JSON processing

## File Viewing (via bash)

- **`bat`** over `cat` — syntax highlighting

## When to Use Built-in Tools vs. CLI

| Task                   | Tool                          |
| ---------------------- | ----------------------------- |
| Read file contents     | built-in `read`               |
| Write/edit files       | built-in `write`/`edit`       |
| Content search         | `rg` via bash                 |
| File search / globbing | `fd` via bash                 |
| Structural code search | `ast-grep` via bash           |
| Text replacement       | `sd` via bash                 |
| Structural refactoring | `ast-grep --rewrite` via bash |

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
