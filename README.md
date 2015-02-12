Bindings Oculus SDK 0.4.4 for Haskell (OS X & Windows 32bit Only)
====

Overview
 Low-level Haskell bindings to Oculus SDK.

## Description
 (T.B.D....)

## Requirement

* [Oculus SDK and runtime](https://developer.oculus.com/)
* (WINDOWS) [libovr_dll_0.4.4.zip](http://www.jspenguin.org/software/ovrsdk/)

## Install

1. install Oculus SDK(0.4.4) and Runtime.
1. `git clone https://github.com/tmishima/bindings-Oculus.git`
1. (WINDOWS) download libovr_dll_0.4.4.zip and unzip anywhere.
1. (OS X) download ovr_sdk_macos_0.4.4.tar.gz and unzip into bindings-Oculus dir.
1. `cabal sandbox init`
1. `cabal install GLFW-b GLUtil linear`
1. copy libovr.dll form (libovr_dll_0.4.4\x86) to bindings-Oculus dir.
1. `cabal clean`
1. (WINDOWS) `cabal configure --extra-include-dirs="(libovr_dll_0.4.4.zip unpack dir)\libovr_dll_0.4.4\dynamic" --extra-lib-dirs="(libovr_dll_0.4.4.zip unpack dir)\libovr_dll_0.4.4\x86"`
1. (OS X) `cabal configure`
1. `cabal build`
1. run sample program.
  1. set Display Mode to "Extend Desktop to the HMD"
  1. run sample program. `cabal run sample1`
  1. set Display Mode to "Direct HMD Access form Apps" (Windows only)
  1. run sample program. `cabal run sample2`           (Windows only)

![screen capture](bindings-Oculus-sample2.png)

## Licence

[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)

## Author

[mishima](https://twitter.com/tty_mishima)
