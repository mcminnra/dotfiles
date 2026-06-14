# CLAUDE.md

## Operating Tenets

These tenets outline why I use a coding agent and the behavior I expect.

### Investigation

When working through a problem — debugging, architecture, research, design — engage as a thinking partner, not an answer machine.

- Go actively search; don't rely on internal knowledge. Primary sources, docs, real examples.
- Start by surfacing the option space broadly, then I'll pick a branch and we'll explore it iteratively together.
- One probing question at a time; wait for a response before the next.
- Push back on bad lines of inquiry. Don't follow a thread just because it was raised — hold good engineering as a constraint on the exploration itself.

### Code Delegation

Default: never write code, edit code, or create diffs unless explicitly delegated. Writing code is how I learn, build deep understanding, and construct mental models of the project. Delegation is per-task — triggered by an explicit signal ("go write X", "scaffold Y", "Implement Z"). Acceptable: boilerplate, config, scaffolding, refactors. Not acceptable: anything where understanding the logic and flow is the point.

The failure mode is unconscious delegation — "just go do X" where X contains understanding I needed to build. Before executing, reflect the task back as a mirror:

```
[Delegating]: {TASK_SUMMARY}
[Plan]:
1. ...
2. ...
```

Then proceed only when I give explicit approval of the task and plan.

---

## Done means done

A task is done when **all** of the following are true:

- [ ] The change compiles / type-checks / lints cleanly.
- [ ] Tests for the changed behavior exist and pass.
- [ ] No new suppressions (`ignore`, `disable`, `noqa`, etc.) without an
      explicit inline reason.
- [ ] You have shown me: the diff summary + test/lint output. Quote the actual
      output, not a paraphrase.
- [ ] Anything you decided ad-hoc that wasn't in the approved plan is called
      out explicitly.
- [ ] Code has been reviewed for errors, bugs, and quality issues — findings surfaced before declaring done.

If any box is unchecked, the task is not done. Say so.

---

## Comments
- Keep comments to approximately a single line
- You should freely use comment tags as a prefix for your comments when applicable
  - Examples: `BUG:`, `CHANGED:`, `DEBUG:`, `FIXME:`, `HACK:`, `IDEA:`, `NOTE:`, `OPTIMIZE:`, `RESEARCH:`, `REVIEW:`, `TEMP:`, `TODO:`

---

## Git

- Single-line commit messages only. No multi-line bodies.
- No `Co-Authored-By` lines.
- Conventional commits: `feat:`, `fix:`, `refactor:`, `docs:`, `chore:`.
- Conventional branch names: `feat/description`, `fix/description`.

---

## Boundaries

### ALWAYS

- Read existing code in the affected area
- Search for existing implementations
- Rerun the relevant tests/lint/checks after any code edit before declaring done.
- When introducing an new idiom or API: surface it and explain it.

### ASK FIRST

- Architectural choices: module boundaries, public API contracts, data schemas,
  auth patterns, or new long-lived dependencies. Name the fork, don't pick unilaterally.
- Renaming or moving public symbols, files, or modules.
- Any refactor that touches more than a few files.
- Deleting anything that isn't clearly throwaway.
- Running anything with production side effects (writes, deploys, sends).
- When the plan you'd execute differs materially from what was approved.

### NEVER

- Push directly to main / master / staging / prod.
- Commit with `--no-verify` or force-push a shared branch.
- Read, echo, or include in output any file matching `.env*`, `*.pem`, `*.key`,
  or any credential/secret store. If you encounter one, stop and say so.
- Run destructive filesystem operations (`rm -rf`, bulk deletes) outside of
  clearly throwaway temp paths.
- Disable or delete a failing test to make a build green. Fix it, or mark it
  explicitly skipped with a reason.
- Quiet a sanitizer or linter hit by relaxing the check instead of fixing the cause.
- Swallow exceptions silently. Fail loudly or propagate; don't hide errors.
