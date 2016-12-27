# Steps

A Haskell programmer needs to do a bunch of tasks. The ordered plan of attack is:

1. Add a list of steps Haskell programmers need to take.
1. Add ways of acheiving those steps (likely with Stack, Cabal, Platform etc)
1. Include disadvantages of those ways.
1. Put the content into a coherent whole, possibly multiple pages.

It's a marathon, not a sprint - making mistakes along the way is expected - please don't judge this page until it is finalised.

The examples are deliberately concrete, referring to specific projects, to identify specific challenges. That will change before the end.

To give specific instructions, I'm using the acronyms "MP" for minimal platform, "S" for Stack. I'm using "Win"/"Lin"/"Mac" when I have reason to believe the instructions are not the same. I've put things that suck in _italics_. Things that are not prefixed are common to all approaches.

## Get "Haskell"

MP-Win: Go to https://www.haskell.org/platform/windows.html. Choose 64bit (_or 32bit_). Run the installer. _Modify your cabal config file._ Start WinGHCi (_which means Win users have a different experience - it's 2009 code hosted on code.google - a terrible impression - and doesn't get indexed by the Start Menu for some reason - let's pretend this didn't happen)._ _Goes through 2 installers (Stack and other)._ _Requires admin._ _Install process is very slow (should benchmark properly)._

## Execute 1+1 in GHCi

MP: On the console type `ghci`. Type 1+1.

## Create a file HelloWorld with `main = putStrLn "Hello world"` and run it.

MP: Save file as HelloWorld.hs. Type `runhaskell HelloWorld.hs`.

## Parse a JSON fragment in GHCi using the aeson library.

MP: `cabal update` (_not doing that takes a long time and says do it twice_). `cabal install aeson` (_watch lots of warnings using the word "unsafe" scroll past_). `ghci`.

```
$ import Data.Aeson
$ :set -XOverloadedStrings
$ decode "[1]" :: Maybe [Int]
```

## Create a new project depending on aeson.

## Run HLint over HelloWorld. Note that haskell-src-exts requires happy to be available.

MP: `cabal install hlint`.

```
hlint Main.hs
```

## Run Hoogle. Note that network has a configure script, which may not always work on Windows.

MP: `cabal install hoogle`.

```
hoogle generate --insecure
hoogle search filter
```

_Insecure because the certificate bundle isn't found._

## Upgrade your version of HLint and Hoogle to the latest released version.

## Use a package directly from github that isn't yet released to Hackage.

## Get a stack trace on errors for your project.

## Profile your project, both heap profiling and time profiling.

## Switch between multiple compiler versions

## View local documentation.
