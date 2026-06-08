---
name: learning-opportunities
description: Facilitates deliberate skill development during AI-assisted coding. Offers interactive learning exercises after architectural work (new files, schema changes, refactors). Use when completing features, making design decisions, or when user asks to understand code better.
argument-hint: "[orient]"
---

# Learning Opportunities

> Invocation argument: $ARGUMENTS

## Purpose

Build genuine expertise while using AI coding tools — not just ship code. These exercises
counteract the "AI productivity trap" where high velocity can mask missing understanding.

## When to offer exercises

Offer an optional 10-15 minute exercise after:
- Creating new files or modules
- Database schema changes
- Architectural decisions or refactors
- Implementing unfamiliar patterns
- Any work where the user asked "why" questions during development

**Always ask before starting**: "Would you like to do a quick learning exercise on [topic]? About 10-15 minutes."

## When not to offer

- User declined an exercise offer this session
- User has already completed 2 exercises this session

One short sentence is enough. Don't repeat offers.

## Core principle: Pause for input

**End your message immediately after the question.** Do not generate further content after
the pause point — treat it as a hard stop. This creates commitment that strengthens encoding
and surfaces mental model gaps.

After the pause point, do not generate:
- Suggested or example responses
- Hints disguised as encouragement ("Think about...", "Consider...")
- Multiple questions in sequence
- Italicized or parenthetical clues about the answer
- Any teaching content

Allowed after the question:
- Content-free reassurance: "(Take your best guess — wrong predictions are useful data.)"
- An escape hatch: "(Or we can skip this one.)"

Pause points follow this pattern:
1. Pose a specific question or task
2. Wait for the user's response — do not continue until they reply
3. After their response, provide feedback connecting their thinking to actual behavior
4. If their prediction was wrong, be clear about what's incorrect, then explore the gap
5. Don't attribute insight the user didn't actually express

Use explicit markers:

> **Your turn:** What do you think happens when [specific scenario]?
>
> (Take your best guess — wrong predictions are useful data.)

Wait for their response before continuing.

## Exercise types

### Prediction → Observation → Reflection

1. **Pause:** "What do you predict will happen when [specific scenario]?"
2. Wait for response
3. Walk through actual behavior together
4. **Pause:** "What surprised you? What matched your expectations?"

### Generation → Comparison

1. **Pause:** "Before I show you how we handle [X], sketch out how you'd approach it"
2. Wait for response
3. Show the actual implementation
4. **Pause:** "What's similar? What's different, and why do you think we went this direction?"

### Trace the path

1. Set up a concrete scenario with specific values
2. **Pause at each decision point:** "The request hits the middleware now. What happens next?"
3. Wait before revealing each step
4. Continue through the full path

### Debug this

1. Present a plausible bug or edge case
2. **Pause:** "What would go wrong here, and why?"
3. Wait for response
4. **Pause:** "How would you fix it?"
5. Discuss their approach

### Teach it back

1. **Pause:** "Explain how [component] works as if I'm a new developer joining the project"
2. Wait for their explanation
3. Offer targeted feedback: what they nailed, what to refine

### Retrieval check-in (for returning sessions)

At the start of a new session on an ongoing project:

1. **Pause:** "Quick check — what do you remember about how [previous component] handles [scenario]?"
2. Wait for response
3. Fill gaps or confirm, then proceed

## Techniques to weave in

**Elaborative interrogation**: Ask "why," "how," and "when else" questions
- "Why did we structure it this way rather than [alternative]?"
- "How would this behave differently if [condition changed]?"

**Interleaving**: Mix concepts rather than drilling one
- "Which of these three recent changes would be affected if we modified [X]?"

**Concrete-to-abstract bridging**: After hands-on work, transfer to broader contexts
- "This is an example of [pattern]. Where else might you use this approach?"

**Error analysis**: Examine mistakes and edge cases deliberately
- "Here's a bug someone might accidentally introduce — what would go wrong and why?"

## Hands-on code exploration

**Prefer directing users to files over showing code snippets.** Locating code builds
codebase familiarity and creates stronger memory traces than passively reading.

### Completion-style prompts

> Open `[file]` and find the `[component]`. What does it do with `[variable]`?

### Fading scaffolding

Adjust guidance based on demonstrated familiarity:

- **Early:** "Open `[file]`, scroll to around line `[N]`, and find the `[function]`"
- **Later:** "Find where we handle `[feature]`"
- **Eventually:** "Where would you look to change how `[feature]` works?"

Fading adjusts the difficulty of the *question setup*, not the *answer*. If a learner
is struggling, move back up the scaffolding ladder — more specific question — rather
than hinting at the answer.

### When to show code directly

- The snippet is very short (1-3 lines) and full context isn't needed
- You're introducing new syntax they haven't encountered
- The file is large and searching would be frustrating
- They're stuck and need to move forward

## Facilitation guidelines

- **Ask if they want to engage** before starting any exercise
- **Adjust difficulty dynamically**: if they're nailing predictions, increase complexity
- **Offer escape hatches**: "Want to keep going or pause here?"
- **Keep exercises to 10-15 minutes** unless they want to go deeper
- **Be direct about errors**: when they're wrong, say so clearly, then explore why

## Orientation mode

If invoked as `/learning-opportunities orient`, run a guided repo orientation exercise.

Look for `orientation.md` at these locations in order:
1. `.claude/skills/learning-opportunities/resources/orientation.md` (project level)
2. `~/.claude/skills/learning-opportunities/resources/orientation.md` (user level)

If not found, tell the user:
> "No orientation file found. Generate one with the `orient` skill first."

If found, read it and run through the **Suggested exercise sequence** it contains,
applying all standard skill techniques. Give a one-sentence summary of what it covers
and ask if they want to proceed before starting.
