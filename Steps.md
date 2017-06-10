# Steps

A Haskell programmer needs to do a bunch of tasks. The ordered plan of attack is:

1. Add a list of steps Haskell programmers need to take.
1. Add ways of acheiving those steps (likely with Stack, Cabal, Platform etc)
1. Include disadvantages of those ways.
1. Put the content into a coherent whole, possibly multiple pages.

It's a marathon, not a sprint - making mistakes along the way is expected - please don't judge this page until it is finalised.

The examples are deliberately concrete, referring to specific projects, to identify specific challenges. That will change before the end.

To give specific instructions, I'm using the acronyms "MP" for minimal platform, "S" for Stack, and "N" for Nix. I'm using "Win"/"Lin"/"Mac" when I have reason to believe the instructions are not the same. I've put things that suck in _italics_. Things that are not prefixed are common to all approaches.

## Get "Haskell"

MP-Win: Go to https://www.haskell.org/platform/windows.html. Choose 64bit (_or 32bit_). Run the installer. _Modify your cabal config file._ Start WinGHCi (_which means Win users have a different experience - it's 2009 code hosted on code.google - a terrible impression - and doesn't get indexed by the Start Menu for some reason - let's pretend this didn't happen)._ _Goes through 2 installers (Stack and other)._ _Requires admin._ _Install process is very slow (should benchmark properly)._

S: Download stack from https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows (_in one of about a million ways (BUG)[https://github.com/commercialhaskell/stack/issues/3207]_). Type `stack setup`.

N: `nix-shell -p ghc` (to use it from a subshell) or `nix-env -i ghc` (to install it in the user's environment).  _Using the name `haskell` instead of `ghc` in either case produces an error._ _Works on Mac/Linux; does not support Windows at all._

## Execute 1+1 in GHCi

MP: On the console type `ghci`. Type 1+1.

S: On the console type `stack ghci`. Type 1+1.

N: Same as MP.

## Create a file HelloWorld with `main = putStrLn "Hello world"` and run it.

MP: Save file as HelloWorld.hs. Type `runhaskell HelloWorld.hs`.

S: Save file as HelloWorld.hs Type `stack runhaskell HelloWorld.hs`

N: Same as MP.

## Create a haskell program and run it as a script (OSX, Linux, BSD only)

S: Create a file HelloWorld.hs:

```haskell
#!/usr/bin/env stack
-- stack --install-ghc runghc
module Main where
main = putStrLn "Hello World"
```

and mark it executable, and run:

```sh
chmod +x HelloWorld.hs
./HelloWorld.hs
```

See [script-interpreter](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter) for more documentation

## Parse a JSON fragment in GHCi using the aeson library.

MP: `cabal update` (_not doing that takes a long time and says do it twice_). `cabal install aeson` (_watch lots of warnings using the word "unsafe" scroll past_). `ghci`.

```
$ import Data.Aeson
$ :set -XOverloadedStrings
$ decode "[1]" :: Maybe [Int]
```

S: `stack ghci --package=aeson` then run the same commands inside `ghci` as for MP.

N: `nix-shell -p 'haskellPackages.ghcWithPackages (p: [ p.aeson ])'`, run GHCi, and then run the same steps inside GHCi as for MP.  _Formulating this expression requires the user to understand a good deal of the nix programming language, and relies on proper escaping of the shell command argument, which is full of spaces and symbols._

## Create a new project depending on aeson.

## Run HLint over HelloWorld. Note that haskell-src-exts requires happy to be available.

MP: `cabal install hlint` then `hlint Main.hs`.

S: 

N: `nix-shell -p 'haskellPackages.ghcWithPackages (p: [ p.hlint ])'`, then `hlint Main.hs`.

## Run Hoogle. Note that network has a configure script, which may not always work on Windows.

MP: `cabal install hoogle`.

```
hoogle generate --insecure
hoogle search filter
```

_Insecure because the certificate bundle isn't found._

N: `nix-shell -p 'haskellPackages.ghcWithPackages (p: [ p.hoogle ])'`, then same as MP, except there is no need for insecure.

## Upgrade your version of HLint and Hoogle to the latest released version.

S: Go to http://stackage.org and look up the latest nightly or LTS. Insert it into your `C:\Users\Neil\AppData\Roaming\stack\global-project\stack.yaml` file against the `resolver` line. Type `stack install hlint` as before.

N: `nix-channel --update`, then exit and re-enter any open nix-shells.  Note: "latest released versions" in this case will mean "latest that have been certified and built by the channel maintainers".  _It is possible to build with newer releases, but requires a solid understanding of Nix._

## Use a package directly from github that isn't yet released to Hackage.

N:

```
# If the user has not installed cabal2nix, they will need to do so with `nix-shell -p cabal2nix` or `nix-env -i cabal2nix`
cabal2nix https://github.com/ryantrinkle/NotOnHackage > NotOnHackage.nix
nix-shell -p 'haskellPackages.ghcWithPackages (p: [ (p.callPackage ./NotOnHackage.nix {}) ])'
```

S:

In the `packages` section of the `stack.yaml`, add the following:

```
- location:
    git: https://path/to/my/repo
    commit: <some commit or branch name>
  extra-dep: true
```


## Get a stack trace on errors for your project.

N: `nix-shell -p '(haskellPackages.override { overrides = self: super: { mkDerivation = args: super.mkDerivation (args // { enableLibraryProfiling = true; enableExecutableProfiling = true; }); }; }).ghcWithPackages (p: [ p.aeson ])'`

_This is a non-starter for anyone without a heavy investment in Nix.  Even if you have the deep knowledge of Nix's Haskell integration necessary to formulate this, it's extremely inconvenient.  In practice, most projects seem to have a flag somewhere in their own configuration that enables this, but it really ought to be made easier upstream._

S: build your project with profiling `stack build --profile` and run the program

## Profile your project, both heap profiling and time profiling.

N: See "Get a stack trace"; then, proceed with standard GHC commands.

## Switch between multiple compiler versions

N: `nix-shell -p haskell.packages.ghc7103.ghc`.  _The syntax is not as clean as the normal one; perhaps ghc7103 could be added as a top-level package in nixpkgs._

S: Stack uses resolvers to compile with certain versions of the compiler.

e.g. to build with ghc 7.8.4:

`stack build --resolver lts-2.22`

or ghc 7.10.3:

`stack build --resolver lts-6.30`

see [stackage](https://www.stackage.org/) for the list of resolver

## View local documentation.

N: `nix-shell -p 'haskellPackages.ghcWithHoogle (p: [ p.aeson ])'` will build a local Hoogle database; local haddocks can be retrieved using `ghc-pkg field $packageName haddock-html`.

## Deploy in a locked down environment

E.g. a company or university, where the system administrators may have rules about distro packages or website access.

N: https://nixos.org/wiki/How_to_install_nix_in_home_(on_another_distribution)

S: Stack install without system priviledges provided its lightweight dependencies requirements are met. you just need to run the command:

`curl -sSL https://get.haskellstack.org/ | sh`

or:

`wget -qO- https://get.haskellstack.org/ | sh`

S-Windows: TODO

## Deploy to many users

E.g. a whole student classroom.

S: See "Deploy in a locked down environment", having no need for user priviledges and a really simple 1 line setup, this can be run by users directly as initial step of a setup/class/etc.
