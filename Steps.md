# Steps

A Haskell programmer needs to do a bunch of tasks. The ordered plan of attack is:

1. Add a list of steps Haskell programmers need to take.
1. Add ways of acheiving those steps (likely with Stack, Cabal, Platform etc)
1. Include disadvantages of those ways.
1. Put the content into a coherent whole, possibly multiple pages.

It's a marathon, not a sprint. Trying to do step 2 before step 1 is likely to result in more work than necessary.

The examples are deliberately concrete, referring to specific projects, to identify specific challenges. That will change before the end.

* Execute 1+1 in GHCi

* Create a file HelloWorld with `main = putStrLn "Hello world"` and run it.

* Parse a JSON fragment in GHCi using the aeson library.

* Create a new project depending on aeson.

* Run HLint over HelloWorld. Note that haskell-src-exts requires happy to be available.

* Run Hoogle. Note that network has a configure script, which may not always work on Windows.

* Upgrade your version of HLint and Hoogle.

* Get a stack trace on errors for your project.

* Profile your project, both heap profiling and time profiling.

* View local documentation.
