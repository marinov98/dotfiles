---
name: prizzler
description: Use this skill when creating or editing pull requests. Generates well-structured PR descriptions following GitHub best practices for reviewability.
---

# prizzler — PR Skill

## When to use

- Creating a new pull request
- Editing/improving an existing PR description
- Reviewing and refining your own PR before submission

## Principles

1. **Small, focused PRs** — Each PR should fulfill a single purpose. Smaller PRs are faster to review, less error-prone, and keep a clearer history.
2. **Explain the why, not the how** — The code _is_ the implementation. The PR body should cover motivation, context, and tradeoffs.
3. **Self-review first** — Review, build, and test your own PR before submitting. Catch typos and errors before others see them.
4. **Provide reviewer guidance** — Tell reviewers where to start, what order to read files in, and what kind of feedback you need (quick look vs deep critique).
5. **Link everything** — Connect PRs to issues, tracking projects, and related PRs so the full context is discoverable.
6. **Review for security** — Check dependency diffs, review code scanning alerts, and flag unresolved risks for reviewer attention.

## Template

Use the template in [TEMPLATE.md](TEMPLATE.md) when writing a PR description. Fill in every section that applies; omit sections (like Projects Affected or Screenshots) when they don't apply.

## Writing the Description

Describe the **net change from main**, not the branch's internal history. Reviewers see `main..HEAD` — they don't care about intermediate steps like "I split X into Y then renamed Z". They care about the final state: what's different, why it's better, and what the new API/structure looks like.

- **Good**: "Module layout reorganized into domain-aligned files. Internal types are now private to the crate."
- **Bad**: "First I merged input.rs into types.rs, then I split it into config.rs and source.rs, then I renamed..."

## Workflow

### Step 1: Sync with main

Ensure local `main` is up to date, then sync the current branch.

```bash
# Fetch latest main
git fetch origin main:main

# Attempt fast-forward first, fall back to rebase
git merge --ff-only main || git rebase main
```

If the rebase rewrote history (commits changed), tests must be rerun before proceeding using the project's test command.

### Step 2: Gather context

```bash
# Understand what changed (now against up-to-date main)
git log --oneline main..HEAD
git diff main...HEAD --stat

# Identify affected packages/areas
# (monorepo detection: check for Cargo.toml, package.json, etc. at multiple levels)
```

### Step 3: Determine if monorepo

Check if the project uses a monorepo structure (workspaces, packages/, etc.). If so, list the affected packages in the Projects Affected section.

### Step 4: Draft the PR body

Fill in the TEMPLATE.md sections:

- **Overview** — 2-3 sentences covering what the PR does and why, at a glance
- **Projects Affected** — only if monorepo
- **Why** — the motivation, problem, or user need (not a rehash of diffs)
- **Approach** — high-level design decisions, tradeoffs, alternatives considered
- **Testing** — what was tested and how
- **Screenshots** — if UI changes
- **Related Issues** — use `Closes #N` syntax
- **Reviewer Notes** — review order, areas of concern, feedback type desired

### Step 5: Self-review checklist

Before presenting the PR:

- [ ] Does the Overview answer both "what" and "why" at a glance?
- [ ] Is the Why section about motivation, not a rehash of the code?
- [ ] Are all related issues linked?
- [ ] Have I reviewed the diff for typos, TODOs, and debug code?
- [ ] Do all tests pass?
- [ ] Is there any security concern worth flagging?

### Step 6: Present to user

Show the user the complete PR body and ask for confirmation before submitting.
