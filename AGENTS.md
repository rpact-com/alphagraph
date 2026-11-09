# Instructions for Coding Agents

These instructions apply to work in the alphagraph repository. Read applicable
subdirectory instructions and [.github/copilot-instructions.md](.github/copilot-instructions.md)
before editing. Keep guidance specific to alphagraph and its existing tooling.

## Engineering priorities and scope

- Prioritize statistical correctness, reproducibility, stable application
  behavior, backward compatibility, and maintainability.
- Inspect the relevant implementation, callers, tests, and documentation before
  changing behavior. Reproduce reported failures where possible and correct
  their cause. Explain uncertainty instead of guessing.
- Make small, coherent changes. Reuse existing helpers, modules, stores, and
  conventions before adding abstractions or dependencies. Avoid unrelated
  refactoring, formatting, and generated-file changes.
- Preserve unrelated local changes and user files. Review the final diff and
  report the resulting behavior, validation performed, and remaining limitations.
- Before changing another repository, read its own `AGENTS.md`. Instructions
  specific to rpact or rpact.tests do not automatically apply to alphagraph;
  alphagraph tests belong in this repository.

## Statistical correctness and project data

- Use `graphicalMCP` as the computational engine. Do not invent, simplify, or
  reinterpret statistical methods. Inspect the relevant engine implementation,
  documentation, and reference examples when a method's behavior is unclear.
- Treat changes to calculations, displayed statistics, validation, saved
  projects, restored state, and exported results as requiring particular care.
  Preserve hypothesis ordering, stable IDs, matrix dimensions and dimnames,
  alpha levels, numerical precision, and rejection decisions.
- Keep the reusable graph in `AlphaGraph`. Keep datasets, raw p-values,
  analysis specifications, and derived results in `AlphaProject`. Preserve the
  distinction between editable symbolic drafts and numerically computable graphs.
- Preserve result invalidation when relevant inputs change, input fingerprints,
  and engine-version metadata. Display only results valid for the current inputs.
- Keep JSON representations and schemas under `inst/schema/` synchronized.
  Preserve round-trip behavior and existing project formats; make compatibility
  changes explicit and cover them with regression tests.

## Shared stores, Shiny, and browser behavior

- Use `AlphaGraphStore` and `AlphaProjectStore` as the shared session state.
  Follow their existing transaction, snapshot, validation, and revision patterns.
  Commit mutations atomically and preserve the previous state when validation fails.
- Render views from canonical state. Avoid rendering that writes back into the
  store, duplicate sources of truth, graph/matrix feedback loops, and observers
  added merely to compensate for unclear reactive dependencies.
- Keep reactive dependencies and initialization order explicit. Review
  event-observer initialization, programmatic input updates, invalidation,
  and synchronization between graph, tables, editors, and analysis outputs.
- Namespace module inputs, outputs, and browser messages. Preserve stable IDs
  and avoid session-specific state in global variables.
- Preserve the bs4Dash layout and established visNetwork/rhandsontable behavior
  unless the task requests a UI change. Keep browser event handling predictable;
  avoid repeated handler registration and unnecessary graph redraws.
- Keep labels, tooltips, notifications, and error messages concise, actionable,
  and statistically accurate. Keep UI, restored state, descriptions, reports,
  and downloads consistent with the same project data.

### Event observers

For new or modified event-driven observers, prefer `shiny::observe()` combined
with `shiny::bindEvent()` using the native pipe `|>` instead of
`shiny::observeEvent()`. Follow this pattern:

```r
shiny::observe(
    {
        projectStore$runShortcut(analysisId, source = "analysis:shortcut")
    },
    label = "run-shortcut-analysis"
) |>
    shiny::bindEvent(
        input$runShortcut,
        ignoreInit = TRUE,
        ignoreNULL = TRUE,
        label = "run-shortcut-analysis"
    )
```

- Specify the event dependencies explicitly in `bindEvent()`.
- Typically set both `ignoreInit = TRUE` and `ignoreNULL = TRUE` explicitly.
  Choose different values when initialization or handling a NULL event is
  required, and explain the reason in a short code comment.
- Give both `observe()` and `bindEvent()` the same descriptive, stable label
  identifying the operation. Include the relevant identifier in labels for
  dynamically created observers.
- When converting an existing observer, preserve its event dependencies,
  initialization, NULL handling, priority, and lifetime behavior. Do not change
  those semantics merely to match the typical defaults above.
- Keep ordinary dependency-driven `observe()` calls when no event binding is
  needed. Avoid converting unrelated observers as part of another change.

## R source formatting and syntax

Apply these rules to new and edited code. Existing violations are not examples
to follow; avoid reformatting unrelated code or changing numerical behavior.

- Use four spaces per indentation level, never tabs.
- Use braces for every `if`, `else if`, `else`, `for`, `while`, and `repeat`
  body, including early returns and conditional expressions in assignments.
  Put the body on separate lines. Write `} else {` and `} else if (...) {`
  on the same line. Apply the rules to nested logic as well.
- Use braced, multiline function bodies, including short functions, R6 methods,
  and anonymous functions. Put `{` on the same line as the end of the signature
  and the closing `}` on its own line. Do not use inline function or loop bodies.
- Prefer explicit `return(value)` in named functions and R6 methods that return
  a value. Preserve intentional invisible returns with `return(invisible(value))`.
  Do not invent return values for functions whose purpose is a side effect.
- Use `<-` for assignment and `=` for named arguments and defaults. Preserve
  `=` where required by R syntax, including R6 member lists.
- Put spaces around assignment, comparison, and binary arithmetic/logical
  operators and around `=` in argument lists. Put spaces after commas and
  control-flow keywords: `if (...)`, `for (...)`, `while (...)`.
- Write `length(x)` and `function(x)` without a space before `(`. Do not put
  spaces just inside parentheses or brackets, around `$`, `@`, `::`, or `:::`,
  or between unary operators and their operands.
- Write one statement per line; do not join statements with semicolons.
  Separate functions and logical steps with blank lines.
- Wrap long signatures and calls at argument boundaries. Use one argument per
  line when helpful and align a separate closing parenthesis with the call.
  Avoid compressing nested logic into a single expression.
- Prefer double-quoted strings, using single quotes when they avoid escaping
  embedded double quotes. Write `TRUE` and `FALSE`, not `T` and `F`.
- Use descriptive `camelCase` names. Preserve established class names, leading
  dots for internal helpers, external API names, and serialized field names.
- Use explicit namespace qualification or maintained imports for dependencies.
  Do not rely on packages attached interactively or by the source-mode app.
- Keep executable R code portable: use Unicode escapes for non-ASCII string
  literals. Preserve the displayed text when replacing a literal with an escape.
- For new maintained R source files, use the existing alphagraph license and
  author header in `R/standalone_utilities.R`. Preserve required third-party
  notices and generator notices.

## Validation and error handling

- Validate inputs at the appropriate model or transaction boundary. Check types,
  lengths, missing/non-finite values, identifiers, and related constraints before
  mutating shared state or invoking the statistical engine.
- Follow alphagraph's existing error-handling conventions. Use clear English
  messages identifying the invalid input and expected requirement; use
  `call. = FALSE` for direct user-input errors where appropriate.
- Preserve useful context from caught errors. Do not silently replace failed
  computations with plausible defaults, empty results, or stale output.
- Keep error handling local to the operation that can fail. Use the existing
  store error and Shiny notification mechanisms; avoid broad handlers that hide
  programming errors or leave partially updated state.

## Documentation and Roxygen

- Update documentation alongside behavior and interface changes, including
  relevant help text, schemas, README, vignettes, and NEWS entries.
- Begin and end every new or edited Roxygen block with an empty `#'` line.
  Keep separator lines free of trailing whitespace.
- Write an explicit `@title` on its own line, with the title on the next line.
  Likewise, write `@description`, `@details`, and `@examples` on their own
  lines above their content. Do not rely on implicit title/description paragraphs.
- Separate logical sections with empty `#'` lines. Keep consecutive `@param`
  entries together. Keep named entries such as `@param x ...`, `@return ...`,
  and `@rdname ...` inline; put `@export` on its own line.
- Use actual Roxygen syntax, without Markdown bold markers around tags or HTML
  entities for blank lines. Use useful, deterministic examples.
- Document new R6 classes and changed public methods/fields, including types,
  defaults, dimensions, optional values, and whether data is supplied or derived.
  Check actual implementation behavior before describing it.
- Respect the mixed documentation workflow: some `man/*.Rd` topics are maintained
  by hand and group multiple aliases, while others are generated from Roxygen.
  Update the authoritative source and preserve unrelated topics. Review generated
  diffs for duplicate aliases, deleted help pages, and incidental rewrites.
- `NAMESPACE` is generated by Roxygen. Maintain imports and exports in the
  Roxygen declarations and regenerate it with `roxygen2::roxygenize()`.
  The compatibility `exportPattern` in `R/alphagraph-package.R` also exports
  alphabetically named helpers: document them or use the established dot-prefixed
  internal naming convention when introducing genuinely internal helpers.
- Parse and check affected Rd files with `tools::parse_Rd()` and
  `tools::checkRd()` when changing help-file structure. Keep alphagraph vignettes
  in the existing `vignettes/` workflow and website configuration.

## Testing and package checks

- Keep R tests in `tests/testthat/` and browser-logic unit tests in `tests/js/`.
  Use the existing testthat and Node test-runner patterns.
- For bug fixes, prefer a focused regression test demonstrating the failure
  before the fix. Test meaningful behavior and edge cases; do not weaken an
  assertion merely to obtain a passing result.
- For numerical changes, compare against established `graphicalMCP` results or
  documented reference examples, with justified tolerances and stable ID mapping.
- For store/module changes, test propagation across views, invalidation, rejected
  mutations, and preservation of edits as relevant. Use `shiny::testServer()`
  and inspect the actual rendered output type when asserting UI content.
- For JavaScript interaction changes, run the affected Node tests and verify
  the actual browser interaction when unit tests cannot establish UI behavior.
- Run focused tests first, then the relevant broader suite. Typical commands
  from the repository root are:

  ```r
  testthat::test_local(filter = "project")
  testthat::test_local()
  rcmdcheck::rcmdcheck(args = "--no-manual", check_dir = "_dev/agents/check")
  ```

  ```powershell
  node --test tests/js/*.test.cjs
  ```

- For package, dependency, namespace, documentation, or resource changes, run
  an appropriate `R CMD check`, including vignettes when affected. Aim for zero
  errors, warnings, and notes. Inspect diagnostic output rather than trusting
  an exit code or issue count alone. Report environmental limitations separately.
- Declare runtime dependencies in `DESCRIPTION` and maintain required imports
  in `NAMESPACE`. Keep development/vignette dependencies in the appropriate
  fields. Avoid reserved R package directories for application resources.
- Resolve packaged resources through the existing resource helpers. Check both
  source-mode and installed-package behavior when changing resource locations.

## Agent working files and Windows tooling

- Store task-specific scratch scripts, exploratory output, logs, temporary
  fixtures, package libraries, and check results under descriptive subdirectories
  of `_dev/agents/`. Create the directory before writing working artifacts.
  Maintained source, documentation, and permanent tests stay in their normal
  locations. Keep working artifacts out of Git and built packages.
- Use repository-relative paths in maintained instructions and scripts. Locate
  the R executable used by the task and obtain `R.version.string` and `R.home()`
  from that installation; do not infer the active version from another executable.
- For Windows workflows that compile native code, verify the matching Rtools
  release against official compatibility guidance and confirm compilation works
  with that R installation. A directory's existence alone is insufficient.
  If the toolchain cannot be verified, explain the R version/path, expected
  Rtools release, and failure before proceeding with dependent compilation.
- If caching a successful toolchain check, use `_dev/windows-preflight.json`
  with status, R version/home, computer name and Windows build/update revision,
  Rtools release/home, and UTC check time. Reuse it only when those environment
  identifiers still match. Invalidate it after a toolchain failure. Pure R,
  documentation, and JavaScript-only edits do not require native compilation.
- Keep `AGENTS.md` and development artifacts excluded from source-package builds.
  Follow the existing GitHub Actions and pkgdown workflows; do not publish or
  deploy as a side effect of local validation.
