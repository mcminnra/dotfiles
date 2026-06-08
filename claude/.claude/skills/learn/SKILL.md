---
name: learn
description: Interactive learning exercise. Starts a skill-building exercise on the given topic, or infers one from context.
argument-hint: "[topic]"
when_to_use: >
  Use when the user says things like "help me understand X", "let's learn X", "I want to
  understand how X works", "walk me through X", "explain X to me", "teach me X",
  "I don't understand X", "how does X work?", "can you quiz me on X", "let's do a learning
  exercise", or after completing a feature/refactor where the user asked "why" questions
  during development.
---

# Learn

> Invocation argument: $ARGUMENTS

## Purpose

Build genuine expertise while using AI coding tools — not just ship code. These exercises
counteract the "AI productivity trap" where high velocity can mask missing understanding.

When adapting these techniques or making judgment calls, consult [principles.md](./resources/principles.md) for the underlying learning science.

## Entry flow

1. **Determine topic**
   - If `$ARGUMENTS` is provided → use it as the topic
   - Else → check conversation context for a clear topic signal (recent code discussed,
     recent task completed)
   - Else → ask: "What topic do you want to explore?"

2. **Rank exercise types**
   Rank all exercise types by relevance to the topic. For each, write one line explaining
   why it fits. Present the full ranked list and ask the user to pick.

3. **Begin the chosen exercise immediately.**

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
- An escape hatch: "(Or say `skip` to move on.)"

Pause points follow this pattern:
1. Pose a specific question or task
2. Wait for the user's response — do not continue until they reply
3. After their response, provide feedback connecting their thinking to actual behavior
4. If their prediction was wrong, be clear about what's incorrect, then explore the gap
5. Don't attribute insight the user didn't actually express

Use explicit markers:

> **Your turn:** What do you think happens when [specific scenario]?
>
> (Take your best guess — wrong predictions are useful data. Or say `skip` to move on.)

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

### Recall drill

1. Read `MEMORY.md` in the current project's memory directory
   (`~/.claude/projects/<project-slug>/memory/MEMORY.md`). Find a concept, pattern, or
   decision the user has previously worked on that's relevant to the current topic.
2. **If memory is sparse or empty**: ask — "What concept do you want to recall-test?"
3. **Pause:** "Without looking it up — what do you remember about how [concept] works?"
4. Wait for response
5. Fill gaps, correct errors, confirm what they got right

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

- **Adjust difficulty dynamically**: if they're nailing predictions, increase complexity
- **Keep exercises to 10-15 minutes** unless they want to go deeper
- **Be direct about errors**: when they're wrong, say so clearly, then explore why
