# Soranoha CLI ergonomics and quiet shared launcher — design

Date: 2026-07-13

## Goal and current behavior

Make Soranoha subcommands self-describing with `babashka.cli` and remove the
JVM environment notice from apps built with `mkCljLauncher`, without changing
successful command behavior.

The empty invocation, `soranoha help`, and `soranoha --help` already print the
global usage text to stdout and return 0. The new CLI work is:

1. replace the handwritten command dispatcher and usage text with one
   `babashka.cli` command table;
2. add `soranoha help <command>` and `soranoha <command> --help`;
3. handle help before positional arity or required-option validation;
4. generate the command index, positional usage, and `build-publication`
   options from that table; and
5. preserve the existing public `run!` return-code API and stream contract.

## Dependency and command table

Add `org.babashka/cli` 0.12.75 as a direct dependency in `abc/deps.edn` and
update the lock. This intentionally upgrades the transitive 0.5.40 artifact:
0.12.75 provides the dispatch help/error hooks used by this design. Transitive
presence is not an acceptable active-code dependency.

Define an ordered `command-table` in `abc.tools.soranoha`. Each entry uses the
library's table form and contains:

- `:cmds`, with the one-token command path;
- `:fn`, an adapter from the library's parsed `{:opts ...}` input to the
  existing command function;
- `:doc`, whose first line supplies the command-index summary;
- `:args->opts`, naming positional arguments in order;
- `:spec`, documenting/coercing/validating positional values and named options;
  and
- `:order` only where explicit option ordering is useful.

Table order defines help order. Positional commands declare every positional
argument through `:args->opts`; their specs mark those values required, so the
same declaration drives invocation, arity validation, and generated usage.
Adapters call existing functions with positional strings in their current
order. Commands that accept no arguments use no `:args->opts` and restrict
unexpected input.

`babashka.cli` supports `-h` automatically when help is enabled. Since adopting
the library makes it free and conventional, both `-h` and `--help` are in
scope. The generated help option may appear in command help. A separate visible
`help` command is not added to the command table.

## Help aliases and dispatch adapter

Call `babashka.cli/dispatch` with `{:prog "soranoha" :help true}`. Its native
forms provide `soranoha --help`, `soranoha -h`, and
`soranoha <command> --help`/`-h`.

A small argument normalizer preserves the existing and requested aliases:

- an empty argument vector becomes `["--help"]`;
- `["help"]` becomes `["--help"]`; and
- `["help" command]` becomes `[command "--help"]`.

Additional arguments after `help <command>` are a usage error rather than being
silently discarded.

Keep `run!` as the unit-testable API returning an integer. Bind the library's
exit/error hooks in this adapter so dispatch errors do not call `System/exit`:

- help prints to stdout and maps to 0;
- unknown/missing command, extra positional input, missing required input, and
  option parse errors print to stderr and map to 2;
- an `ExceptionInfo` raised by an invoked command retains the current command
  failure mapping; and
- a successful command's existing integer result is returned unchanged.

Only `-main` calls `System/exit`, as it does today. Characterization tests pin
the exact mappings before the dispatcher is replaced because `babashka.cli`
defaults to status 1 for dispatch errors.

## `build-publication` migration

Move the six `build-publication` option declarations into its command-table
`:spec` using `babashka.cli` keys:

- required paths/values: `:aozora-root`, `:config`, `:snapshot-date`, and
  `:output-root`;
- boolean `:replace`; and
- non-negative integer `:concurrency`, defaulting to 0.

The command adapter passes the validated option map to a focused
`build-publication!` entry point. Path resolution against `ABC_INVOCATION_PWD`,
the release-policy check, concurrency resolution, and publication workflow stay
in `abc.tools.soranoha-build-publication`.

This removes that namespace's separate `clojure.tools.cli` parsing declaration;
the command-table spec becomes the single owner for parsing and generated help.
Characterization tests cover required options, `--replace`, concurrency
coercion/validation, relative paths, and option ordering before the migration.

## Quiet shared Clojure launcher

The edit target is `mkCljLauncher` in `abc/flake.nix`, shared by Soranoha,
`validate-design-bundle`, `materialize-import`, `materialize-publication`, and
the other `mkCljApp` apps. All of those apps should become quiet.

`mkCljLauncher` stops exporting `JAVA_TOOL_OPTIONS`, because HotSpot announces
that variable on startup. It passes the equivalent
`-J-Duser.home=<cljDepsCache>` directly to Clojure. The direct property remains
necessary: on the supported Linux runtime, changing `HOME` alone does not
change JVM `user.home`. `HOME`, `CLJ_CONFIG`, `GITLIBS`, and the writable
temporary `CLJ_CACHE` remain unchanged.

The processes spawned by `build-publication` are the adapter/parser/converter
chain plus `git` and `7zz`, not child JVMs, so environment propagation is not
needed.

The separate `JAVA_TOOL_OPTIONS` uses in `presentationLauncher`,
`cljSandboxEnv`, and the presentation font-cache/drift builder are intentionally
out of scope. They are not used by `nix run .#soranoha`.

## Tests and validation

Tests cover:

- global help through empty, `help`, `--help`, and `-h` forms;
- per-command help through `help <command>`, `<command> --help`, and
  `<command> -h`, all on stdout with status 0;
- generated positional names and complete `build-publication` option help;
- fixed-arity help bypassing required/arity validation;
- unknown commands/help targets and malformed invocations using stderr/status 2;
- command adapters preserving positional order and successful return codes;
- `build-publication` parsing and validation equivalence; and
- command exceptions retaining their existing status behavior.

A focused Nix source/evaluation test targets `mkCljLauncher`: its generated app
launcher omits `JAVA_TOOL_OPTIONS` and invokes Clojure with
`-J-Duser.home=<cljDepsCache>`. It is not a repository-wide assertion because
the three intentional uses remain.

Run focused Clojure tests, dependency-lock validation, and Nix checks, followed
by `just validate-migration` if those pass.

## Out of scope

- Migrating other ABC CLIs from `clojure.tools.cli`.
- Shell-completion generation, multi-word commands, or command aliases beyond
  `help <command>`.
- Changing command names or publication workflow behavior.
- Suppressing application errors or unrelated JVM output.
