# Copilot instructions for alphagraph

- `alphagraph` is a Shiny application using the `graphicalMCP` package.
- Prioritize statistical correctness, reproducibility, stable UI behavior, backward compatibility, and consistency with the underlying `graphicalMCP` methodology.
- Treat all changes affecting calculations, simulations, displayed statistics, bookmarking, session restore, downloads, reports, or validation-related behavior as high risk.
- Do not invent, simplify, or reinterpret statistical methodology. If behavior is unclear, inspect related `graphicalMCP` logic, application code, tests, and documentation before changing anything.

## Architecture and application behavior

- Follow the existing repository architecture and surrounding file patterns.
- Prefer small, targeted patches over broad refactors.
- Reuse existing helpers, modules, reactive values, and established app patterns before introducing new abstractions.
- Avoid changes that silently alter default values, UI initialization, reactive timing, restored state, generated reports, or downloaded outputs.
- Keep behavior consistent across UI, server logic, saved sessions, restored sessions, reports, and exported files.

## Shiny, reactivity, and bookmarking

- Keep reactive dependencies explicit, stable, and easy to understand.
- Avoid unnecessary observers, hidden side effects, duplicated reactive logic, and fragile event chains.
- Be especially careful with:
  - initialization order
  - `observeEvent()` / `eventReactive()` interactions
  - propagation of restored values into UI controls
  - synchronization between UI inputs and internal reactive state

## bs4Dash and UI conventions

- Preserve the existing bs4Dash-based structure and layout unless a UI redesign is explicitly requested.
- New UI elements should fit the existing naming, layout, and interaction patterns.
- Keep labels, tooltips, popovers, help links, and modal dialogs concise, precise, and statistically correct.
- Avoid unnecessary changes to layout, spacing, box structure, tab structure, or navigation behavior.
- Prefer maintainable UI code over clever shortcuts.

## Reports, downloads, and generated output

- Treat reports, exported files, and downloadable results as part of the public interface.
- Do not silently change report content, field names, formatting, default text, or included statistics unless explicitly requested.
- Keep generated output consistent with the visible UI state and with the underlying `rpact` results.
- If report logic changes, review whether tests, help text, and release notes must also be updated.

## R code style

- Follow the style of the surrounding file and existing repository conventions.
- Prefer explicit, readable, maintainable R code over clever or overly compact code.
- Use `camelCase` for object names, function names, reactive values, IDs where appropriate, and internal helpers. Do not introduce new `snake_case` names unless required by an external interface.
- Avoid unnecessary new dependencies.

## Testing

- Any non-trivial change should be considered incomplete without checking whether tests need to be added or updated.
- For bug fixes, prefer adding or updating a regression test first, then implementing the fix.
- Prefer focused, deterministic tests with clear intent.
- Do not weaken tests just to make them pass.
- When relevant, consider:
  - unit tests
  - shinytest2 snapshots and interactions
  - bookmarking and restore behavior
  - regression coverage for UI-state propagation
  - report and download behavior

## Documentation and release notes

- Keep user-facing documentation synchronized with the implementation.
- When changing user-visible behavior, review and update as needed:
  - help text
  - tooltips and popovers
  - modal text
  - user documentation
  - NEWS / release notes
  - report text
- Use precise technical language. Do not oversell features or methodological implications.

## Preferred Copilot behavior

- Inspect related files and existing patterns before editing.
- Prefer minimal patches that fit the current architecture.
- Explicitly flag uncertainty instead of guessing.
- When proposing a change, consider whether tests, help text, documentation, reports, downloads, bookmarking behavior, or release notes also need updates.
- Avoid changes that make the application noticeably slower, more fragile, or harder to debug without clear benefit.
