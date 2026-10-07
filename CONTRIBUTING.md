# Contributing

Please open a GitHub issue before proposing a substantial API change. Keep
calendar arithmetic pure and deterministic, preserve explicit error results,
and add regression coverage for every behavior change.

Before submitting a change, run:

```sh
cabal check
cabal build all
cabal test all
cabal haddock all
```
