# Agents Guide for llm-test

## Build/Test/Lint Commands

- **Test all**: `eldev test`
- **Run one scenario**: `eldev test llm-test/<group-slug>/<index>`
  - `group-slug` is the slugified YAML `group:` name: lowercase, with non-alphanumeric characters replaced by `-` (for example, `basic editing` -> `basic-editing`).
- **Lint**: `eldev lint`
- **Compile**: `eldev compile`

## End-to-End Scenario Testing

- `testscripts/` are meant to be exercised end-to-end with a real LLM provider.
- Do **not** replace scenario execution with a fake, mocked, or scripted provider unless the user explicitly asks for that.
- If end-to-end scenario tests need a provider for command-line use, prefer wiring it through the environment variable `LLM_TEST_PROVIDER_ELISP` rather than hard-coding local setup in tests.
- In this repo, missing provider configuration for end-to-end scenario tests should fail clearly rather than skipping.
- Keep repository guidance generic: do not add developer-specific provider setup, local file paths, or personal environment details to this file.
- When a scenario fails end-to-end, first check whether the YAML spec matches real Emacs behavior. If the behavior requires extra setup, fix the scenario/setup itself rather than adding test-only logic that bypasses the LLM path.

## Code Style Guidelines

- **File format**: Emacs Lisp with `lexical-binding: t` in first line
- **Headers**: Include standard copyright, GPL license, commentary section
- **Naming**: `llm-test-` prefix for public functions, `llm-test--` for private
- **Variables**: Use `defcustom` for user options with `:type` and `:group`
- **Tests**: Use ERT (`ert-deftest`), place in `test/` directory or `*-test.el` files
- **Formatting**: Standard Emacs Lisp indentation, max ~80 chars per line

## Project Structure

- `llm-test.el` — Main library (YAML parsing, subprocess control, agent loop, ERT registration)
- `testscripts/` — Sample YAML test specifications that should run end-to-end
- `Eldev` — Build configuration

## Project-Specific Pitfalls

- `llm-test.el` ends with `-test.el`, so Eldev can misclassify it as a test file unless the main/test filesets are set explicitly. Preserve the explicit fileset configuration in `Eldev`.
- Keep `README.org` updated when changing provider configuration or command-line testing flow, especially anything involving `LLM_TEST_PROVIDER_ELISP`.

## Dependencies

- `llm` — LLM provider interface
- `yaml` — YAML parsing
- `ert` — Emacs test framework (built-in)
- `cl-lib` — Common Lisp extensions (built-in)

## Architecture

1. **YAML Parser**: Reads `.yaml` test specs into `llm-test-group` / `llm-test-spec` structs
2. **Emacs Subprocess**: Launches `emacs -Q` and communicates via eval
3. **Agent Loop**: LLM agent with tools (eval-elisp, send-keys, etc.) interprets tests
4. **ERT Integration**: Each test spec becomes an ERT test via dynamic `ert-deftest`
