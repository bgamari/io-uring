# io-uring

A simple set of [io-uring](https://kernel.dk/io_uring.pdf) bindings in Haskell. See Main.hs for an example on how to use it. The example code can be run with `cabal run`.

## Requirements

Since the `io_uring` interface was only added to the Linux kernel in version 5.1 (released around May 2019) so it's possible that you are running a kernel version that supports it. From the command line, `uname -r` will tell you the kernel version of your system.

## Warning

It is NOT advised to `cabal install --lib` this repo to get the libraries installed as non-hidden modules. Part of the bindings include C-- code that prevents the GHCi bytecode interpreter from even starting up.
 