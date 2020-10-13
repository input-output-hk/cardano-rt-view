# Building RTView

RTView is a program implemented in the Haskell programming language. There are different ways how to build it from scratch, but this guide describes two recommended methods:

1. Using `cabal`;
2. Using `nix`.

## Prerequisites

You will need:

1. An x86 host (AMD or Intel) with at least 2 cores, 8GB of RAM, and at least 10GB of free disk space;
2. A recent version of Linux, macOS, or Windows (see clarification below);
3. [git](https://git-scm.com/) program.

## Get the source code

Clone RTView repository:

```
git clone https://github.com/input-output-hk/cardano-rt-view.git
```

## Building using cabal

### Install GHC and cabal

It is recommended to use [ghcup](https://www.haskell.org/ghcup/) to install GHC and cabal on your computer. Run this command (as a user other than `root`):

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

and follow the onscreen instructions. Please do not forget to restart your terminal to activate changes in your `PATH` variable.

Now install and activate the required GHC version:

```
ghcup install ghc 8.6.5
ghcup set ghc 8.6.5
```

Check GHC version:

```
ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.6.5
```

### Building RTView

Now go to the repository directory and run:

```
cabal build all
```

After the build is finished, you can run RTView using this command:

```
cabal exec -- cardano-rt-view
```

## Building using nix

Nix is a powerful package manager for Linux and other Unix systems that makes package management reliable and reproducible. Please note that you **cannot** use `nix` on Windows (use `cabal` instead).

The quickest way to install Nix is to run the following command (as a user other than `root` with `sudo` permission):

```
curl -L https://nixos.org/nix/install | sh
```

Make sure to follow the instructions output by this script.

After `nix` is installed, go to the repository directory.

### Build RTView program

Use the following command to build RTView program:

```
nix-build -A cardano-rt-view
```
After that, you will see a link `result` in the repository directory. This link points to the program. For example, on Linux, you will see something like this:

```
result -> /nix/store/xnff4qfrpxrsgwnfwc2i6jx5pj8bq3xi-cardano-rt-view-exe-cardano-rt-view-*
```

That directory contains `bin/cardano-rt-view` program.

### Build RTView release package

You can also build a complete release package for your platform. It is similar to the archive available in the [stable releases](https://github.com/input-output-hk/cardano-rt-view/releases). To do it, please use the following commands:

1. `nix-build release.nix -A cardano-rt-view-linux-release` - for Linux;
2. `nix-build release.nix -A cardano-rt-view-darwin-release` - for macOS.

After that, you will see a link `result` in the repository directory. This link points to the package. For example, on Linux, you will see something like this:

```
result -> /nix/store/fa0c47r743ra9nslpl86zp4pjpz8sygs-cardano-rt-view-*-linux-x86_64
```

That directory contains an archive `cardano-rt-view-*-linux-x86_64.tar.gz`.
