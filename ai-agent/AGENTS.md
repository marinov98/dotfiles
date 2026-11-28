# Global Instructions

## Engineering Philosophy

**Correctness by construction over correctness by convention.** Encode invariants in types, interfaces, and program structure rather than relying on memory, comments, or caller discipline.

Apply these principles to new code and intentional redesigns. Do not rewrite working architecture solely to impose them.

### Type-driven design

- **Types are the primary design artifact; implementation follows from them.** Model the domain, its states, transitions, and invariants first. Design signatures before function bodies.
- **Make illegal states unrepresentable.** Prefer precise domain types over primitives, loosely related optional fields, and implicit invariants.
- **Parse, don't validate.** Unstructured input becomes a trusted domain type once, at the boundary. Preserve those guarantees internally.
- **Preserve type guarantees.** Do not weaken types, introduce unchecked casts, or bypass the type system merely to make an implementation compile.
- Encode correctness statically wherever practical. Enforce remaining invariants at runtime.
- Types should simplify reasoning, not demonstrate sophistication. No speculative generics or type-level machinery without meaningful guarantees.

### Program design

- Pure core, impure shell. IO, clock, and randomness live at the edge.
- Prefer immutability and expressions over mutation and statements.
- Expected failures are return values; exceptions are for the unrecoverable. Respect established project and language contracts.
- **Data-oriented design.** Plain data structures, behavior in functions. Model the data and its transformations first; let the code follow the shape of the data, not the other way round.
- Composition over inheritance.
- Dependencies passed in, not constructed or located.

## Tiebreakers

- **Do more with less.** Minimize the work and complexity required to achieve the outcome. Prefer omission and deletion over addition, without sacrificing correctness or clarity.
- **Quality over implementation effort.** Do not sacrifice correctness, simplicity, robustness, or long-term maintainability to save implementation effort. Speculative flexibility is not quality.
- **Simplest direct path first.** For one-off or infrequent work, do the thing end to end. No wrappers, control planes, policy layers, or automation until the direct path hits a concrete blocker or a repeat need that justifies the machinery.
- **Abstractions must earn their existence.** Abstract to hide meaningful complexity, express domain knowledge, or isolate a genuine boundary. Syntactic similarity is not semantic equivalence. Prefer duplication over the wrong abstraction.
- **Deep modules, not more modules.** Concentrate necessary complexity behind simple interfaces. Reduce what callers must know. Avoid shallow wrappers, pass-through layers, and unnecessary indirection. When cleaner and simpler conflict, simpler wins.
- **Challenge the abstraction.** When requirements conflict with the existing design, reconsider the abstraction before adding exceptions, flags, or compatibility layers.

## Execution

- **Plan first when warranted.** Investigate and plan before significant implementation involving uncertainty, risk, or architectural decisions. Execute straightforward work directly.
- **Reproduce before fixing.** Start with the failure as the user experiences it. Narrow the reproduction only when it preserves the failure mechanism. Fix causes, not symptoms.
- **Challenge the premise.** When evidence contradicts the approach or fixes repeatedly fail, STOP and revisit the underlying assumptions rather than accumulating patches.
- **Prove completion.** Verify actual changed behavior using the strongest practical evidence. Compilation and linting alone do not establish behavioral correctness. State what could not be verified.
  - Run `shellcheck` on any non-trivial shell script written or modified before declaring completion.
- **Don't launder unrelated fixes.** Do not modify unrelated code to silence failures. Establish whether a failure is caused by the current change using the diff, dependencies, and baseline verification when necessary. Report confirmed pre-existing failures separately.
- **Never hand-edit generated files.** Regenerate generated changelogs, lockfiles, and anything marked auto-generated.
- Resolve routine, reversible implementation decisions independently. Ask when ambiguity materially affects intended behavior, public contracts, irreversible operations, or scope.
- **Ask before spawning subagent swarms.** Workflows, ultracode, and large fan-outs need the tradeoff stated and explicit approval. Single targeted agents are fine.
- Don't write memory files unless asked.

## Comments

Good code is self-documenting. Comments are not enforceable. Never write inline or block comments;

Docstrings are not comments: they are interface documentation. Write them only where the project already uses them, on public interfaces only, in the project's established style. Capture what the code cannot express — the abstraction, contract, and why — never what the implementation already says.

## Communication

Terse. No preamble, restating the request, or summarizing visible tool output. One sentence where one works, none where the diff speaks. Answer what was asked; expand when asked for detail.
