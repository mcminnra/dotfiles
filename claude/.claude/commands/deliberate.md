---
name: deliberate
description: Structured iterative brainstorming and planning protocol for features, refactors, and technical designs. Use when the user says "brainstorm", "think through", "deliberate", begins describing a task with background context like "I want to... because... so that...", or references an existing spec file for implementation planning. Produces either an implementation plan (Plan Mode), a spec document, or a decision summary.
---

# Deliberate

A structured protocol for iterative planning through brainstorming, critical feedback, and convergence.

## Trigger

Activate when the user:

- Uses keywords: "brainstorm", "think through", "deliberate"
- Describes a task with motivation: "I want to... because... so that..."
- Provides a feature/refactor description with background context
- References an existing spec file as context (e.g., "read this spec", "I'm implementing phase 2 of...")

## Phase 1: Reflect & Clarify

If the user references an existing spec file, read it first. If they mention a specific phase, focus your understanding and questions on that phase's scope — treat the spec as the "brief" rather than asking the user to re-explain it.

After the user describes their task (or after reading the referenced spec):

1. **Build context** — Read the files and code paths referenced in the user's prompt, spec, or background context. Understand the existing architecture before reflecting or asking questions. If the spec lists file paths, interfaces, or integration points, read them.
2. **Reflect back** your understanding of what they described — restate the problem, goal, and constraints in your own words so they can verify you got it right
3. **Ask 3-5 clarifying questions** using A/B/C options:
   - Each question should have labeled options (A, B, C)
   - Include your recommendation and a brief reason why
   - Focus on ambiguities, unstated assumptions, and architectural decisions
   - Use inline text format (not the AskQuestion tool) so the user can write freeform responses

**Example format:**

```text
**Q1: How should we handle X?**
- A) Option one — tradeoff description
- B) Option two — tradeoff description
- C) Option three — tradeoff description
→ Recommendation: B, because [reason]
```

## Phase 2: Iterative Refinement Loop

After the user answers your questions (they will often add extra context and new ideas alongside their answers):

Respond with **three things**, in this order:

1. **Critical feedback** on their answers — This is essential. Even if you agree with their choices, push back with gotchas, edge cases, risks, or things they may not have considered. Challenge assumptions constructively. This is what makes the plan robust.

2. **Decision summary** — A running table or list of all decisions made so far. Keep it updated each iteration so nothing gets lost.

3. **Follow-up questions** — 2-4 more questions in the same A/B/C format with recommendations. These should dig deeper based on the new information from their answers. **Continue numbering from where the previous round left off** (e.g., if Phase 1 ended at Q4, the first follow-up is Q5). This allows the user to reference earlier questions unambiguously.

**Repeat this loop** until convergence. Convergence means:

- The user explicitly says they're satisfied ("looks good", "let's do it", "I'm happy with this")
- OR you recognize there are no meaningful open questions left — in which case, propose moving forward and confirm

## Phase 3: Exit Path

At convergence, determine the exit path:

### Path A: Implement Now

If the user wants to implement immediately:

1. Suggest switching to **Plan Mode** to create a detailed implementation plan
2. In Plan Mode, produce a phased implementation plan with clear steps
3. After the plan is approved, switch back to **Agent Mode** to implement
4. **If implementing from a spec**, update the spec file as you go:
   - Update the status (e.g., `📋 PROPOSED` → `🚧 IN PROGRESS`)
   - Check off completed items in the Success Definition (`- [ ]` → `- [x]`)
   - When all items in a phase are done, update status to `✅ PHASE N COMPLETE` or `✅ COMPLETE`

### Path B: Write a Spec

If the user wants to defer implementation (they'll usually say "write a spec" or mention a spec folder):

1. Read the [spec template](https://gist.github.com/benmvp/0e4e24ce9228ab794e99dfeb205107b6) for the document structure
2. Ask the user where to write the spec (or use the folder they mention — it likely already contains specs to match style)
3. Read 1-2 existing specs in the target folder to match the local conventions (tone, depth, section ordering)
4. Write the spec file incorporating all decisions from the deliberation. Ask where to write the spec.

### Path C: Just the Decisions

If the user just wants the output of the deliberation without a plan or spec, provide a final comprehensive summary of all decisions, rationale, and open items.

## Guidelines

- **Don't rush convergence.** The iterative loop is the value. Let the user drive when to stop.
- **Don't be a yes-man.** Critical feedback should be genuine — surface real risks, not just validate choices.
- **Track everything.** Decisions made in early rounds shouldn't be forgotten in later rounds.
- **Stay concise in questions.** Each question should be focused on one decision point.
- **Adapt question count.** Early rounds may need 3-4 questions. As you converge, 1-2 is fine.