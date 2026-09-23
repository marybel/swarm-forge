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
