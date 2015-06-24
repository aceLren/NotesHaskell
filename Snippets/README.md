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

That puts the dependencies in the sandbox so cabal can find them when you run the repl.  To do that on any of the example files::

```haskell
cabal repl
ghci>:l Example.hs
```

And you'll be able to run the defined functions.
