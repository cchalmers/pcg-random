## pcg-random

Minimal (for now) interface to the PCG random number generator [http://www.pcg-random.org].

Initial benchmarks:

```
benchmarking pcg/Word32
time                 2.773 ns   (2.731 ns .. 2.828 ns)
                     0.997 R²   (0.996 R² .. 0.999 R²)
mean                 2.856 ns   (2.802 ns .. 2.938 ns)
std dev              221.8 ps   (172.7 ps .. 281.4 ps)
variance introduced by outliers: 88% (severely inflated)

benchmarking mwc/Word32
time                 5.395 ns   (5.313 ns .. 5.507 ns)
                     0.997 R²   (0.995 R² .. 0.998 R²)
mean                 5.584 ns   (5.453 ns .. 5.763 ns)
std dev              548.0 ps   (363.8 ps .. 795.5 ps)
variance introduced by outliers: 92% (severely inflated)

benchmarking mersenne/Word32
time                 2.905 ns   (2.859 ns .. 2.964 ns)
                     0.997 R²   (0.996 R² .. 0.998 R²)
mean                 2.990 ns   (2.937 ns .. 3.074 ns)
std dev              218.4 ps   (167.6 ps .. 339.2 ps)
variance introduced by outliers: 87% (severely inflated)
```
