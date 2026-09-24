# Agent instructions

Do not test the text of prompts with an automated unit or acceptance test.
That includes constitution articles, role prompts, Tool Startup, and generated
instruction files. Prompt wording is not production behavior to pin with
`str/includes?`, Gherkin, or any other automated check.

Run dry4clj on Babashka scripts with this repo's `bb dry` task. dry4clj
only scans `.clj`, `.cljc`, `.cljs`, and `.cljd` files, so pointing it
straight at `.bb` sources silently finds nothing and reports a false-clean
result. `bb dry` stages `.clj` copies of the `.bb` files under
`swarmforge/scripts` in `./tmp/dry`, runs dry4clj on those copies, prints
the findings, and exits non-zero when it finds any duplication candidate.

Run CRAP via crap4clj, using this repo's `bb crap` task (bb.edn:91-99) to
invoke it: crap4clj.core with `--source-root swarmforge/scripts` and
`--coverage-command "bb coverage:bb"`. `coverage:bb` makes two crap4clj
cloverage passes (`bb coverage:pass test`: `-s test -r clojure.test`, scoped
with `--test-ns-regex` to `coverage-in-process-test` so subprocess-covered
code isn't miscounted as untested; `bb coverage:pass spec`: `-s spec -r
speclj`), each writing its own LCOV under `target/coverage/<pass>/`, then
merges them with `swarmforge/scripts/lcov_merge.bb` into
`target/coverage/lcov.info`. Both the clojure.test suite and the Speclj
specs under `spec/` count toward each function's CRAP score. `bb crap` is
crap4clj itself invoked through Babashka's task runner, not a homegrown
proxy for it.

Keep the existing `test/*.clj` clojure.test suite as-is; it originates
upstream, not from this fork. Write any new test as a Speclj spec, per the
constitution's Speclj-only rule for Clojure and Babashka projects.
