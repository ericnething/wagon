Programming Exercises for Wagon (wagonhq.com)
=============================================

Installing
----------

~~~
cabal sandbox init
cabal install --only-dependencies

~~~

Exercise 1: Streaming Statistics
--------------------------------

Build and run:

~~~
cabal build ex1
./dist/build/ex1/ex1 < input.csv

~~~

To see time and memory usage statistics:

~~~
./dist/build/ex1/ex1 +RTS -s -RTS < input.csv

~~~

The input file `input.csv` contains 1 million rows of data. In my tests, it uses a total of 3 megabytes of memory and runs in ~4 seconds. The non-streaming (in-memory) version used on the order of 1 gigabyte of memory and runs an order of magnitude slower.

Exercise 2: Lazy Loading/Rendering
----------------------------------

Build and run:

1. `cabal run ex2`
2. Open `src/ex2/ex2.html` in your web browser

