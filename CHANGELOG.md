### 0.1.3.4

* Fix byte array bugs in System.Random.PCG.Pure

### 0.1.3.3

* Added System.Random.PCG.Pure module.

### 0.1.3.2

* Added System.Random.PCG.Fast.Pure module.

### 0.1.3.1

* Added `withFrozen` function.

### 0.1.3.0

* Use [entropy](http://hackage.haskell.org/package/entropy) package for
  system random generator.

### 0.1.2.0

* Allow primitive-0.6.

### 0.1.1.0

* Add uniformB, a function to generate a bounded random number in [0,b)
  range. This preforms significantly faster than uniformR (0,b-1).

* Add type specific versions for uniformR and uniformB.

### 0.1.0.1

* Fix bug when dealing with `Word` and `Int` types.

