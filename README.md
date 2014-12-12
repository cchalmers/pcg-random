## pcg-random

[![Build Status](https://travis-ci.org/cchalmers/pcg-random.svg)](https://travis-ci.org/cchalmers/pcg-random)

Haskell bindings to the PCG random number generator http://www.pcg-random.org.

> PCG is a family of simple fast space-efficient statistically good algorithms for random number generation with better-than-typical cryptographic security

Implements the standard multiple stream generator as well as the fast, single and unique variants.

The api is very similar to [mwc-random] but the pcg generator appears to be faster. There is also a pure interface via the [random] libray.

[mwc-random]: https://hackage.haskell.org/package/mwc-random
[random]: http://hackage.haskell.org/package/random
