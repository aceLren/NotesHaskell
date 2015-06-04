Day 3
===

WAT
---

[WAT](https://www.destroyallsoftware.com/talks/wat)

---

1.  Implicits
2.  Identity
3.  Mutability
4.  Scope

Tic Tac Toe
---

```python
row = [''] * 3
board = [row] * 3
board[0][0] = 'X'
board
```

```matlab
sum(0)
sum([])
sum(zeros(0,1))
sum(zeros(1,0))
sum(zeros(2,0))
sum(zeros(0,2))
```
---

Dijkstra Letter
---

[UTexas](http://www.cs.utexas.edu/users/EWD/OtherDocs/To%20the%20Budget%20Council%20concerning%20Haskell.pdf)

>   Colleagues from outside the state (still!) often wonder how I can survive in a place like Austin, Texas, automatically assuming that Texas’s solid conservatism guarantees equally solid mediocrity. My usual answer is something like “Don’t worry. The CS Department is quite an enlightened place, for instance for introductory programming we introduce our freshmen to Haskell”; they react first almost with disbelief, and then with envy —usually it turns out that their undergraduate curriculum has not recovered from the transition from Pascal to something like C++ or Java.

---

>   A fundamental reason for the preference is that functional programs are much more readily appreciated as mathematical objects than imperative ones, so that you can teach what rigorous reasoning about programs amounts to.

---

>   Finally, in the specific comparison of Haskell versus Java, Haskell, though not perfect, is of a quality that is several orders of magnitude higher than Java, which is a mess (and needed an extensive advertizing campaign and aggressive salesmanship for its commercial acceptance). It is bad enough that, on the whole, industry accepts designs of well-identified lousiness as “de facto” standards. Personally I think that the University should keep the healthier alternatives alive.


Functions And Types
---

[Haskell Evolution](http://willamette.edu/~fruehr/haskell/evolution.html)

---

Smarter Types
---

![Haskell](images/zoidberg1.png)
![Python](images/zoidberg2.png)


What is the type for this?
---

```haskell
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
```
. . .

```haskell
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
```

Currying
---

```haskell
addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z
```

What is the type for this?

```haskell
addThree 2
```

Not Casting, Type Inference
---

```haskell
1 == 1.0
(1 :: Int) == 1.0
(1 :: Int) == (round 1.0)
```

Functions
---

```haskell
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"
```

Recursion Everywhere

```haskell
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)
```

What's Wrong With This?
---

```haskell
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"
```

What about this?
---

```haskell
fib :: Int -> Int
fib n = fib (n-1) + fib (n-2)
fib 0 = 1
fib 1 = 1
```

Constructors
---

```haskell
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
```

Guards
---

```haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0
```

```haskell
...  
where bmi = weight / height ^ 2  
      (skinny, normal, fat) = (18.5, 25.0, 30.0)
```

Also Functions
---

Kind of like private member functions

```haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2
```

Let .. In
---

```haskell
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h =
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
```

Like where, except part of the expression:

```haskell
[let square x = x * x in (square 5, square 3, square 2)]

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
```

Case
---

Pattern matching works on function parameter, this works anywhere:

```haskell
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
```

So sometimes people do this:

```haskell
case () of
  _ | x < 15 -> ...
  _ | x < 25 -> ...
  _ | x < 50 -> ...
  _          -> ...
```

-XMultiWayIf

```haskell
if  | x < 15 -> ...
    | x < 25 -> ...
    | x < 50 -> ...
    |
```  

Types
---

[info](http://stackoverflow.com/questions/991467/haskell-type-vs-newtype-with-respect-to-type-safety)

1.  Type = alias
2.  newtype = compile time help
3.  data = wrapped type


Example
---

```haskell
data MetricUnit = Meter | Liter | KiloGram deriving (Show, Eq)
data ImperialUnit = Yard | Gallon | Pound deriving Show
data Measurement = MetricMeasurement Double MetricUnit
                 | ImperialMeasurement Double ImperialUnit
                   deriving Show

-- "m" "L" "kg" - guards and pattern match (needs Eq)
symbol :: MetricUnit -> String
symbol = undefined

-- To make an instance of Show typeclass manually
-- instance Show MetricUnit where
--    show Meter = undefined
--

convert :: Measurement -> Measurement
convert = undefined
-- MM m -> yard 1.0936
-- MM L -> gallon 0.2642
-- MM kg -> pound 2.2046
-- IM yard -> .9144 m
-- IM gallon -> 3.7854 liter
-- IM pund -> 0.4536 kg

-- try invariant convert (convert m)
```

Typeclass
---

These are similar to interfaces.  I implement equality or "show" functionality
and you can compare or serialize to string.  For normal sum types,
the Haskell compiler is smart and can figure it out.  Just append "deriving
(Eq, Show)" like in the example.  Manually it looks like this:

```haskell
instance Show MetricUnit where
    show Meter = "m"
    show Liter = "L"
    show KiloGram = "kg"

instance Eq MetricUnit where
    Metric == Metric = True
    Liter == Liter = True
    KiloGram == KiloGram = True
    _ == _ = False
```

Type variables
---

```haskell
head :: [a] -> a
fst :: (a,b) -> a
```

```haskell
data Fruit = Apple | Banana | Orange deriving Show
data Point3 a = a a a
data Point3 a = Point3 { x::a, y::a, z::a }
```

Type Trouble
---

Many people have trouble with types and values with same name.

It's menumonic.

TypeClasses
---

![Types](images/classes.gif)

---

![TypeClassOPedia](images/Typeclassopedia-diagram.png)


Etc
---

Common Type Classes
---

```haskell
:t (==)
:t (>)
:t fromIntegral
maxBound :: Int
```

-   (): unit - NOT NULL
    -   [good explanation](http://stackoverflow.com/questions/16892570/what-is-in-haskell-exactly)
-   Bool: boolean
-   Char: character, single quotes
-   Int: bounded
-   Integer: unbounded
-   Float: real floating point, single precision
-   Double: real floating point, double precision

---

-   Eq: support == /=
-   Ord: >, <, >=, <=
-   Ordering: GT, LT, EQ
-   Show: present as string
-   Read: convert from string
-   Enum: sequentialy ordered members
    -   (), Bool, Char, Ordering, Int, Integer, Float and Double
    -   try succ
-   Bounded: upper and lower
    -   Int, Char, Bool
-   Num: act like numbers (show and eq)
-   Integral: Int and Integer
-   Floating: Float and Double

Variables
---

Can't do this:

```haskell
x = 1
...
x = 2
```
