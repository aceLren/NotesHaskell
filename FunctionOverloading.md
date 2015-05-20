Function Overloading
====================

Problem
-------

Often we want a function to be polymorphic either on the input, or in
the return value. This is also called function overloading (similar to
[multiple dispatch discussed
here](http://programmers.stackexchange.com/questions/125084/multiple-dispatch-vs-function-overloading))

C\# and other languages you define the same function with different
typed parameters. [In haskell it looks like
this](http://stackoverflow.com/questions/6636107/polymorphism-in-haskell)

This could also be referred to as [ad-hoc
polymorphism](https://wiki.haskell.org/Polymorphism#Ad-hoc_polymorphism).

Not OK
------

```haskell
split :: String -> [String]                      -- splits on whitespace
split :: Char -> String -> [String]              -- splits on the given character
split :: [Char] -> String -> [String]            -- splits on any of the given characters
split :: (Char -> Bool) -> String -> [String]    -- splits using a function that tells you when
```

Better
------

```haskell
words :: String -> [String]                        -- splits on whitespace
splitOn :: Char -> String -> [String]              -- splits on the given character
splitsOn :: [Char] -> String -> [String]           -- splits on any of the given characters
splitWith :: (Char -> Bool) -> String -> [String]  -- splits using a function that tells you when
```

Even Better
-----------

```haskell
class Fooable a where
    foo :: a -> Int

instance Fooable Int where
    foo = id

instance Fooable Bool where
    foo _ = 42
```

Bonus
-----

```haskell
class Barable a where
    bar :: Int -> a

instance Barable Int where
    bar x = x + 3

instance Barable Bool where
    bar x = x < 10
```

Rectangle Example
----

How do you represent this in haskell:

```cpp
class Rectangle
{
    private:
        int length;
        int width;
    public:
        Rectangle()
        {
            length = 0;
            width = 0;
        }
        Rectangle(int x)
        {
            length = x;
            width =0;
        }
        Rectangle ( int x , int y)
        {
            length = x;
            width = y;
        }
};
```

Haskell Way
----

```haskell
point :: Rectangle
point = Rectangle 0 0

line :: Length -> Rectangle
line l = Rectangle l 0

square :: Int -> Rectangle
square a = Rectangle a a
```

Signature Way
----

```haskell
{-# LANGUAGE FlexibleInstances #-}
class MakeRectangle a where
  rectangle :: a

instance MakeRectangle Rectangle where
  rectangle = Rectangle 0 0

instance MakeRectangle (Length -> Rectangle) where
  rectangle l = Rectangle l 0

instance MakeRectangle (Length -> Width -> Rectangle) where
  rectangle = Rectangle
```
