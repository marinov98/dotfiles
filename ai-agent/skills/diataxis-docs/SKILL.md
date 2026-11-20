---
name: diataxis-docs
description: Write or revise technical documentation using the Diátaxis framework. Use when the user asks to write, draft, restructure or review docs, guides, READMEs, references, tutorials or explanations. Breaks a topic into one document per Diátaxis type it needs, proposes that set, then writes each document to its type's invariants, stating only verified facts.
---

# Diátaxis Documentation

A document has one type and one job. A topic usually needs several documents. Break the topic down, get the set confirmed, write each document to its type's invariants, verify every fact.

## 1. Propose

### Decompose the topic

Before writing anything, list every reader need the topic creates. Classify each need with two questions:

|                                       | Serves **action** (doing) | Serves **cognition** (knowing) |
| ------------------------------------- | ------------------------- | ------------------------------ |
| Reader is **acquiring** skill (study) | Tutorial                  | Explanation                    |
| Reader is **applying** skill (work)   | How-to guide              | Reference                      |

Classify by the reader's situation, never by their seniority. A senior engineer learning a new system is studying. A junior engineer completing a task is working.

Group the needs by type. Each type that has needs becomes one document. A type with no real need gets no document. Do not create documents to fill the quadrants.

Before proposing a new document, check the existing docs. If one already covers a need, extend it.

### Present the set

Present every proposed document in this form, followed by the links between them, and wait for confirmation:

```
Type: <tutorial | how-to | reference | explanation>
Reader: <who, and what they are doing right now>
Goal: <one sentence: what the reader can do or knows after reading>
Title: <title>
Location: <path>
New or existing: <new | extends path>

Links: <document> → <document>: <why the reader follows this link>
```

Never merge types into one document. Write the documents in the order the user confirms. If none is given, write reference first, because the others link to it.

## 2. Write

### Invariants for every type

- Every statement is a fact verified against the source: code, config, command output, or the user's own words. Do not write a fact you have not verified. Ask the user, or leave it out.
- No hedging: no "typically", "usually", "might", "should generally", "it's worth noting". If a statement is conditional, state the condition.
- No preamble, no summary, no closing remarks. The first sentence does work. The last sentence does work.
- One idea per paragraph. One action per step.
- Each section depends only on what came before it. Never point forward to fill a gap.
- Content that belongs to another type is a link, never an inline digression.
- Match the voice, headings and file layout of existing docs in the repo.
- Use real names from the codebase: paths, variables, commands, resource names.
- Code blocks are copy-pasteable and correct as written.

### Tutorial — a lesson

- Title names what the learner will build or achieve.
- The author is responsible for the outcome: every step works exactly as written, in order, from a stated starting point.
- Each step produces a visible result. Show the expected output.
- No choices, no alternatives, no explanation beyond one sentence. Link to explanation.
- Ends when the learner has a working result.

### How-to guide — a recipe

- Title is "<Verb> <object>" and completes "How to…".
- Open with one sentence of scope and one link to the relevant reference.
- **Before you start**: preconditions as a list. State them; do not teach them.
- Numbered steps, in the order the work is done. Each step: the action, the exact file or command, the expected result.
- Warnings sit at the step where the risk occurs.
- Assume competence. Do not explain the tools.
- Ends when the goal is reached, with how to confirm it.

### Reference — a description

- Mirrors the structure of the thing it describes: one section per variable, resource, command or output.
- Tables over prose. Neutral, complete, consistent.
- Describe; never instruct. No steps, no opinions, no rationale.
- Every value is traceable to the source file it came from.

### Explanation — a discussion

- Title is "Why…" or "About…" and names the topic.
- Answers why: design decisions, constraints, trade-offs, history, how the parts relate.
- No steps, no exhaustive specs. Link to how-to and reference.
- Opinions are allowed only when they are attributed decisions ("We chose X because Y").

## 3. Verify

Do not report the document as done until all of these are true:

- [ ] Each document matches its type in the confirmed proposal. No paragraph belongs to another type.
- [ ] No fact is repeated across documents. Each fact lives in one document, and the others link to it.
- [ ] Every fact traces to a source you read in this session.
- [ ] Removing any sentence would lose information.
- [ ] Read top to bottom, each section follows from the previous one.
- [ ] Every link resolves, and the docs build passes if the repo has one (e.g. strict build).
