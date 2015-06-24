Snippets
===

Overview
---

Here are some easy to play around with examples.  To use this do:

```bash
cd Snippets/
cabal sandbox init
cabal install (whatever dependency gets complained about)
```

Then any time you want to run an example:

```haskell
cabal repl
ghci>:l Example.hs
```

Then you'll be able to run the defined functions.
