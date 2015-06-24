Intro to Haskell and Functional Programming
===

Overview
---

This is the course repository for the SFU intro class.  It will be a "today I learned" sort of repo with links, examples, and class notes.  Class notes are the Day... slides.

This is mostly from:

-   [Intro to Haskell](http://shuklan.com/haskell/)
-   [LYAH](http://learnyouahaskell.com/)
-   [Functors, Applicatives, and Monads in Pictures](http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html)
-   [Starting with Haskell](https://www.fpcomplete.com/school/starting-with-haskell)
-   [Functional Programming by Examples](https://github.com/caiorss/Functional-Programming)
-   [Gists](https://github.com/kqr/gists)

The scaffold for simple web apps etc will live here as well.

slides
---

To generate slides, first you need to pull the revealjs code into your folder and install pandoc.  Just clone: https://github.com/hakimel/reveal.js.git

Then type:

    pandoc -t revealjs -s Day1.md -o slides/Day1.html --self-contained --slide-level=2 --highlight-style=zenburn --variable theme="simple"
