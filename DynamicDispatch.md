Simplify Life
=============

Dynamic Dispatch
----------------

[Dynamic dispatch in
Haskell](http://stackoverflow.com/questions/13106683/dynamic-dispatch-in-haskell)

Caveat; after some reading it appears dynamic dispatch happens at
runtime, a lot of haskell stuff is figured out at compile so I'm not
sure this is entirely accurate. The idea is, not sure about the name.

First some Java showing dynamic dispatch:

------------------------------------------------------------------------

    // You have an interface
    public interface Comparator<T> {
        int compare(T o1, T o2); // virtual (per default)
    }

    // You implement it
    public class MyComparator implements Comparator<String> {
        private final int _n;

        MyComparator(int n) {
            _n = n;
        }

        int compare(String s1, String s2) {
            return s1.charAt(_n) - s2.charAt(_n);
        }
    }

    // Now you can use it
    Collections.sort(myList, new MyComparator(5));

In Haskell
----------

    // Just use higher order functions
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]

    myComparator :: Int -> (String -> String -> Ordering)
    myComparator n = \s1 s2 -> (s1 !! n) `compare` (s2 !! n)
    -- n is implicitly stored in the closure of the function we return

    foo = sortBy (myComparator 5) myList

Haskell's In Java
-----------------

    public void <T> sortBy(List<T> list, Ordering FUNCTION(T, T) comparator) { ... }

    public (Ordering FUNCTION(String, String)) myComparator(int n) {
        return FUNCTION(String s1, String s2) {
            return s1[n].compare(s2[n]);
        }
    }

    public void foo() {
        sortBy(myList, myComparator(5));
    }

More Elaborate Now
------------------

    public class Widget {
        public void onMouseClick(int x, int y) { }
        public void onKeyPress(Key key) { }
        public void paint() { }
        ...
    }

    public class MyWidget extends Widget {
        private Foo _foo;
        private Bar _bar;
        MyWidget(...) {
            _foo = something;
            _bar = something;
        }
        public void onMouseClick(int x, int y) {
            ...do stuff with _foo and _bar...
        }
    }

In Haskell
----------

Store info in closures, not an extra type:

    data Widget = Widget {
        onMouseClick :: Int -> Int -> IO (),
        onKeyPress   :: Key -> IO (),
        paint        :: IO (),
        ...
    }

    constructMyWidget :: ... -> IO Widget
    constructMyWidget = do
        foo <- newIORef someFoo
        bar <- newIORef someBar
        return $ Widget {
            onMouseClick = \x y -> do
                ... do stuff with foo and bar ...,
            onKeyPress = \key -> do ...,
            paint = do ...
        }
