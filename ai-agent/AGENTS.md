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

# Philosophy

Always attempt to apply the following principles when designing, refactoring
or adding a new feature to a code base.

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

# Additional Tooling

- `jq`/`yq` to read JSON/YAML.
- Edit tool for targeted edits. `sd` (`-s` for literals) for pattern
  replacements across files; `sed` only for address ranges
- `shellcheck` any non-trivial shell script before calling it done.

## Comments

Good code is self-documenting. Comments are liabilities in ai agents
as they are not enforceable. Never write inline or block comments;

Docstrings are not comments: they are interface documentation.
Write them only where the project already uses them, on public
interfaces only, in the project's established style. Capture what
the code cannot express — the abstraction, contract, and why — never
what the implementation already says.

## Communication

- Terse. No preamble, no restating my request back to me, no summarizing
  tool output I can already see. One sentence where one works, none where
  the diff speaks. Answer what was asked; expand when I ask for detail.

- **Plan first.** For any non-trivial task (3+ steps or an architectural decision), plan
  the approach before writing significant code. If something goes sideways, STOP and re-plan
  rather than pushing forward.
