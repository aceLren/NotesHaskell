Day 7
===

Pitch
---

Something I think will be HUGE over the next 10 years:

[Distributed / Parallel Processing](https://youtu.be/UMbc6iyH-xQ?t=20m14s)

Randoms
---

-   Liquid Types
-   [Tidal](http://tidal.lurk.org/)
-   [Games](http://helm-engine.org/)
-   [Plot R](http://code.haskell.org/~aavogt/ipopt-hs/examples/Spline1.hs)
-   [More R](https://hackage.haskell.org/package/Rlang-QQ-0.1.0.1/docs/RlangQQ.html)

Morphisms
---

Problem - give me all rule names:

```javascript
thing = {name:"Selection A", groups:[{name:"Group A", rules:[{name:"Rule A"},{name:"Rule b"}]}]};
```
. . .

```javascript
[].concat.apply([],sel.groups.map(function(gp){ return !gp.rules ? [] : gp.rules.map(function(rule){ rule.name = gp.name; return rule; }); }))
```

Compare With
---

```haskell
concat . map (map name . rules) $ groups selectionA
```

Data as it is
---

[Monoids, semigroups, etc](https://izbicki.me/blog/gausian-distributions-are-monoids)

Overview
---

-   Finish Monads
-   QuickCheck
-   Example Project

Monads
---

I have new versions of WriterExample and StackExample, lets
work through them.

Kitchen Sink
---

Lets walk through the Example project!

Feedback!!!
---

Please
