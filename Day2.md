Day 2
===

Overview
---

Functional and speed.  What do you define as speed?  What kind of speed
is important to you?

[Benchmarks Game](http://benchmarksgame.alioth.debian.org/u64q/compare.php?lang=ghc&lang2=python3)

---

[Comparison of F# and C# in the wild](http://fsharpforfunandprofit.com/posts/cycles-and-modularity-in-the-wild/)


![Code vs Dependencies](../images/top_level_and_dependencies.png)

---

![Top vs Cycles](../images/top_level_and_cycles.png)

Essentially
---

```csharp
public class CustomerName
{
    public CustomerName(string firstName,
       string middleInitial, string lastName)
    {
        this.FirstName = firstName;
        this.MiddleInitial = middleInitial;
        this.LastName = lastName;
    }

    public string FirstName { get; private set; }
    public string MiddleInitial { get; private set; }
    public string LastName { get; private set; }
}
```

VS
---

```fsharp
type CustomerName(firstName, middleInitial, lastName) =
    member this.FirstName = firstName
    member this.MiddleInitial = middleInitial
    member this.LastName = lastName
```

```haskell
data CustomerName = CustomerName
    {firstName     :: Text
    ,middleInitial :: Text
    ,lastName      :: Text }
```

Rest of Course
---

What is interest?  We'll start doing things in an additive way.  More interested
in data processing type things?  Or maybe network?

Haskell
---

-   .hs
-   .lhs
-   ghci
-   :info (:i)
-   :type (:t)
-   :r (reload)
-   :load (load script name)
-   :edit (uses editor)
-   :? (list all commands)

Info Meld
---

-   [GHCI](http://ghc.io/)
-   [FP Complete](https://www.fpcomplete.com/school/starting-with-haskell)
-   [What I Wish I Knew](http://dev.stephendiehl.com/hask/) - try Text
-   [Functional Programming](https://github.com/caiorss/Functional-Programming)
-   [LYAH](http://learnyouahaskell.com/chapters)
-   [EdX](https://courses.edx.org/courses/DelftX/FP101x/3T2014/info)
    -   [Lecture repo](https://github.com/fptudelft/FP101x-Content)
-   [Cheat Sheet](http://cheatsheet.codeslower.com/CheatSheet.pdf)
-   [Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929/index.html)

Get our feet wet
---

We use &#923; or  &lambda;

``` haskell
λ > 2 + 15
λ > 59 * 100
λ > 5 / 2
λ > (-4) + 5

λ > True && False
False
λ > not (True && True)
False

λ > 5 == 5
True
λ > 5 /= 5
False
λ > "hello" == "hello"

λ > 5 == "hello"
ERROR!
```

Basic Types
---
-    Bool
-    Char
-    String
-    Int (fixed-precision)
-    Integer (arbitrary-precision)
-    Float

Expressions
---

>   Expressions have a value, while statements do not. If you can pass it as an argument to a function, it's an expression. If you can't, it's a statement. Control flow constructs in C-derived languages are generally statements (you can't pass an 'if {}' block as a function parameter). Consequently, those languages tend to provide an expression form for 'if' (like the ternary operator). Of course, the whole point of many functional languages is that everything has a value, and that functions are first-class values, which means that everything is an expression. Statements usually appear in imperative languages, where you wish to write commands that don't necessarily return a value. - Lambda the Ultimate

Operation vs Denotational
---
-   Operational semantics: Translates code to abstract machine statements
-   Denotational semantics: Translates code to mathematical expressions

Simple Function
---

Ticks for infix, ' identify strict vs non.

```bash
echo "doubleMe x = x + x" > example.hs
ghci
```
```haskell
λ > :l example.hs

λ > doubleSmall x = if x > 100 then x else x * 2
λ > conanO'Brien = "It's a-me, Conan O'Brien!"

λ > factorial n = product [1..n]
λ > average ns = sum ns `div` length ns
```

Prefix vs Infix
---

Almost everything in Haskell is a function, and every function can be prefix or infix:

``` haskell
λ > 1 + 1
λ > (+) 1 1
λ > min 9 10
λ > 9 `min` 10
λ > read "10" * 2

abs
mod
even

```

Prelude
---

[Hackage](https://hackage.haskell.org/) and [Stackage](http://www.stackage.org/)

What is [Prelude](https://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html)?

Functional is almost as much about lists as functions.

[Lists](https://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#g:13)

![List Monster](../images/listmonster.png)


Lists
---

If you're indexing you're probably doing it wrong.

-   head
-   tail
-   take
-   drop
-   length (O(n))
-   sum
-   product
-   (++)
-   reverse

Reverse
---

```haskell
rev [] = []
```
. . .

```haskell
rev xs = last xs : rev (init xs)
rev xs = rev (tail xs) ++ [head xs]
rev (x:xs) = rev xs ++ [x]
```

Last
---

Implement last

. . .

last xs = head ( drop (lengt xs - 1) xs)
last xs = xs !! (length xs - 1)

Comprehensions
---

### Simple List Comprehension

```haskell
> [x^2 | x <- [1..10]]
[1,4,9,16,25,36,49,64,81,100]

>  [ odd x | x <- [1..9]]
[True,False,True,False,True,False,True,False,True]

```

### Comprehensions with multiple generators

Comprehensions with multiple generators, separated by commas.
The generators are x <- [1, 2, 4] and y <- [4,5].

```haskell
> [(x, y) | x <- [1, 2, 4], y <- [4,5]]
[(1,4),(1,5),(2,4),(2,5),(4,4),(4,5)]

> [(x-y, x+y) | x <- [1, 2, 4], y <- [4,5]]
[(-3,5),(-4,6),(-2,6),(-3,7),(0,8),(-1,9)]

> [(x/y, x*y) | x <- [1, 2, 4], y <- [4,5]]
[(0.25,4.0),(0.2,5.0),(0.5,8.0),(0.4,10.0),(1.0,16.0),(0.8,20.0)]
```

### Function Inside List Comprehension

```haskell

> let f x y = sqrt(x^2 + y^2)

> [ f x y | x <- [1, 2, 4], y <- [4,5]]
[4.123105625617661,5.0990195135927845,4.47213595499958,5.385164807134504,5.656854249492381,6.4031242374328485]

```

### Comprehension with Guards

Guards or filter is a boolean expression that removes elements that would
otherwise have been included in the list comprehension. They restricts the values
produced by earlier generators.

Even number sequence

```haskell

> [x | x <- [1..10], even x]
[2,4,6,8,10]

>  [x | x <- [1,5,12,3,23,11,7,2], x>10]
[12,23,11]

> [(x,y) | x <- [1,3,5], y <- [2,4,6], x<y]
[(1,2),(1,4),(1,6),(3,4),(3,6),(5,6)]


```

Odd Number sequence

```haskell

> [x | x <- [1..10], odd x]
[1,3,5,7,9]
```


Number factors and Prime Numbers

```haskell

> let factors n = [ x | x <- [1..n], mod n x == 0]
>
> factors 15
[1,3,5,15]
>
> factors 10
[1,2,5,10]
>
> factors 100
[1,2,4,5,10,20,25,50,100]
>
> factors 17
[1,17]
> factors 19
[1,19]

> let prime n = factors n == [1, n]
>
> prime 17
True
> prime 19
True
> prime 20
False
>

{- Get all prime numbers until number n -}

> let primes_n n = [ x | x <- [1..n], prime x]
>
> primes_n 10
[2,3,5,7]
> primes_n 100
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
>

```

Ranges
---

```haskell
λ > [1..20]  
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]  
λ > ['a'..'z']  
"abcdefghijklmnopqrstuvwxyz"  
λ > ['K'..'Z']  
"KLMNOPQRSTUVWXYZ"  

λ > [2,4..20]  
[2,4,6,8,10,12,14,16,18,20]  
λ > [3,6..20]  
[3,6,9,12,15,18]

data MyDataType = Foo | Bar | Baz deriving (Enum, Show)
λ > succ Foo
λ > pred Baz
```

Tuples
---

Which right triangle that has integers for all sides and all sides equal to or smaller than 10 has a perimeter of 24? First, let's try generating all triangles with sides equal to or smaller than 10:

. . . hint

```haskell
λ > let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]
```


Practice
---

[Haskell Basics](https://www.fpcomplete.com/school/starting-with-haskell/introduction-to-haskell/1-haskell-basics)


End
---

BELOW HERE ARE NOTES AND THINGS I HAVENT QUITE FINISHED YET

Fixity
---


| Precedence    | Left associative   | Non-associative    | Right associative |
| ------------- | ----------------   | ---------------    | ----------------- |
| 9	            | !!                 |                    | .                 |
| 8             |                    |                    | ^,^^, **          |
| 7             | *, /, `div`, `mod`,|                    |                   |
|               | `rem`, `quot`		 |                    |                   |
| 6             | +, -               |                    |                   |
| 5             |                    |                    | :, ++             |
| 4	            |                    |==, /=, <, <=, >,   |	                  |
|               |                    |>=,`elem`, `notElem`|                   |
| 3             |                    |                    |	&&                |
| 2             |                    |                    |	&#124;&#124;      |
| 1             | >>, >>=		     |                    |                   |
| 0			    |                    |                    | $, $!, `seq`      |

No definition, infixl 9 assumed.

Example
---

    (^^!) :: (Num a, Integral b) => a -> b -> a
    a ^^! b = a ^ b

    (^^^) :: (Num a, Integral b) => a -> b -> a
    a ^^^ b = a ^ b

    infixl 9 ^^!
    infixr 9 ^^^

    data Test = Test String deriving (Eq, Show)

    (>:) :: Test -> Test -> Test
    (Test a) >: (Test b) = Test $ "(" ++ a ++ " >: " ++ b ++ ")"

    infixr 6 >:

    (<:) :: Test -> Test -> Test
    (Test a) <: (Test b) = Test $ "(" ++ a ++ " <: " ++ b ++ ")"

    infixl 6 <:

    (?:) :: Test -> Test -> Test
    (Test a) ?: (Test b) = Test $ "(" ++ a ++ " ?: " ++ b ++ ")"

    infix 6 ?:


    main = do
        print $ show $ 4 ^^! 3 ^^! 2
        print $ show $ 4 ^^^ 3 ^^^ 2

        print $ (Test "1") >: (Test "2") >: (Test "4")
        print $ (Test "1") <: (Test "2") <: (Test "4")
        print $ (Test "1") ?: ((Test "2") ?: (Test "4"))


Folds
---

The recursion for foldr f x ys where ys = [y1,y2,...,yk] looks like

`f y1 (f y2 (... (f yk x) ...))`

whereas the recursion for foldl f x ys looks like

`f (... (f (f x y1) y2) ...) yk`




Function Names
---

Lowercase
Type uppercase

different namespaces

Mild hungarian, xs is list of xs, two ss, list of lists.

White space is significant:
Same columns

Function Application
---

Usually `f(a,b) + c d` would mean: f with a and b params, c space d mult

Haskell:

    f a b + c * d

Function application binds stronger than others.

    f a + b is f(a) + b

Math vs Haskell
---
Math | Haskell
--- | ---
f(x)      | f x
f(x,y)    | f x y
f(g(x))   | f (g x)
f(x,g(y)) | f x (g y)
f(x)g(y)  | f x * g y

. . . Lighter syntax


Types
---

Algebraic Data Types
---


thing :: type


Lists: Polymorphic

[False, True, False] :: [Bool]
['a','b','c'] :: [Char]
[['a'],['a','b']] :: [[Char]]

Tuples:

(Flase, True) :: (Bool, Bool)
(False, 'a', True) :: (Bool, Char, Bool)
(False, ('a',False)) :: (Bool, (Char, Bool))

Function
---

Mapping of one type to another:

not :: Bool -> Bool
isDigit :: Char -> Bool

Currying - CLOSURES!
---
Arrow is right associative.
Application is left associative.
All functions have one param.
Any with more actually return functions.

    mult :: Int -> Int -> Int -> Int
    // Is actually
    mult :: Int -> (Int -> (Int -> Int))

    mult 3 2 5
    // Is actually
    (((mult 3) 2) 5)

is actually

-   return a function that multiplies by 3 and 2 other values
-   return a function that multiplies by 3, 2 and one other value
-   multiply 3, 2, and 5

Why Currying?
---

Partial application.

try it

Parentheses
---

2 ^ 3 * 4
2 * 3 + 4 * 5
2 + 3 * 4 ^ 5

A
---

(2 ^ 3) * 4
(2 * 3) + (4 * 5)
2 + (3 * (4 ^ 5))

Error?
---

N = a 'div' length xs
    where
        a = 10
       xs = [1,2,3,4]

A
---

n = a `div` length xs
    where a = 10
         xs = [1,2,3,4]

Last
---

last xs = head ( drop (lengt xs - 1) xs)
last xs = xs !! (length xs - 1)
