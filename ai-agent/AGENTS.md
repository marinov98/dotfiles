# Tiebreakers

- **Quality over development cost.** At a fork, weight simplicity,
  robustness, and long-term maintainability. Cheap-to-build is not a
  reason.
- **Simplest direct path first.** For one-off or infrequent work, do the
  thing end to end. No wrappers, control planes, policy layers, or
  automation until the direct path hits a concrete blocker or a repeat
  need that justifies the machinery.
- **Complexity budget goes into interfaces, not layers.** When "cleaner"
  and "simpler" conflict, simpler wins. Deep modules, not more modules.
- **Reproduce before fixing.** Start a bug fix by reproducing it the way
  the user hits it — end to end, not at the unit level — so the fix
  lands on the real cause.
- **Don't launder unrelated fixes.** A lint, type, or test failure in a
  file the change doesn't touch gets reported as pre-existing, not
  silently fixed in the same commit. Confirm with
  `git log --oneline -1 -- <file>` before calling it pre-existing.
- **Never hand-edit generated files.** CHANGELOG.md, lockfiles, anything
  marked auto-generated — regenerate instead.
- **Ask before spawning subagent swarms.** Workflows, ultracode, and
  large fan-outs need the tradeoff stated and explicit approval. Single
  targeted agents are fine.
- **Don't write memory files unless asked.**

# Design

- Types and signatures before code; make illegal states unrepresentable.
- Parse, don't validate — unstructured input becomes a precise type
  once, at the boundary.
- Pure core, impure shell. IO, clock, and randomness live at the edge.
- Prefer immutability and expressions over mutation and statements.
- Expected failures are return values; exceptions are for the
  unrecoverable.
- **Data-oriented design.** Plain data structures, behavior in
  functions. Model the data and its transformations first; let the code
  follow the shape of the data, not the other way round. Composition
  over inheritance.
- Dependencies passed in, not constructed or located.
- Project convention beats every line above.

# Tooling

**Required:** use these by default — they're the first choice for their jobs.

| Tool            | Purpose                   | Key Advantage                                   |
| --------------- | ------------------------- | ----------------------------------------------- |
| `sd`            | Text replacement          | Cleaner syntax than sed; `-F` literal           |
| `scc`           | Codebase overview         | LOC, complexity, language breakdown; respects `.gitignore` |
| `ast-grep (sg)` | Structural code transform | AST patterns + `--rewrite`; respects `.gitignore` (disable: `--no-ignore vcs`) |
| `jq`            | JSON processing           | Pipe from scc, git, package managers            |
| `shellcheck`    | Shell validation          | Catches bugs before they ship                   |
| `yq`            | YAML/INI processing       | Query/edit with comment preservation            |

## Search Strategy

1. Locate files by name or path — prefer fuzzy search when available
2. Scope by extension or directory before content search
3. Search content with context lines around matches
4. Use structural search for code patterns (functions, classes, AST)
5. Use codebase stats to understand composition
6. Built-in `read` for deep dives into specific files

## Pipelines

- `scc --format json | jq '.[] | {lang: .Name, files: .Count}'` — structured queries
- `sd 'old' 'new' file` — simple replacement (no escaping needed)
- `sd -F '$var' 'val' file` — literal replacement (metacharacters safe)
- `ast-grep -p 'pat' --rewrite 'new' -l lang` — structural code transformation
  - `ast-grep` Patterns must be **syntactically complete code**:
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

## Comments

Good code is self-documenting. Comment only what the code cannot say:
design intent that didn't survive into the syntax, the contract of an
abstraction, or a level of detail the reader is missing. A comment that
restates the line below it is worse than none.

## Communication

- Terse. No preamble, no restating my request back to me, no summarizing
  tool output I can already see. One sentence where one works, none where
  the diff speaks. Answer what was asked; expand when I ask for detail.

- **Plan first.** For any non-trivial task (3+ steps or an architectural decision), plan
  the approach before writing significant code. If something goes sideways, STOP and re-plan
  rather than pushing forward.
