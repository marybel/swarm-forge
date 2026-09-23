# Agent instructions

Do not test the text of prompts with an automated unit or acceptance test.
That includes constitution articles, role prompts, Tool Startup, and generated
instruction files. Prompt wording is not production behavior to pin with
`str/includes?`, Gherkin, or any other automated check.

Run dry4clj on Babashka scripts by making `.clj` copies of the `.bb` files
under `./tmp/dry` first, then running dry4clj against those copies. dry4clj
only scans `.clj`, `.cljc`, `.cljs`, and `.cljd` files, so pointing it
straight at `.bb` sources silently finds nothing and reports a false-clean
result.

Run CRAP via crap4clj, using this repo's `bb crap` task (bb.edn:79-86) to
invoke it: crap4clj.core with `--source-root swarmforge/scripts` and
`--coverage-command "bb coverage:bb"`, where `coverage:bb` runs crap4clj's
own cloverage wrapper scoped with `--test-ns-regex` to
`coverage-in-process-test` so subprocess-covered code isn't miscounted as
untested. `bb crap` is crap4clj itself invoked through Babashka's task
runner, not a homegrown proxy for it.

Keep the existing `test/*.clj` clojure.test suite as-is; it originates
upstream, not from this fork. Write any new test as a Speclj spec, per the
constitution's Speclj-only rule for Clojure and Babashka projects.
