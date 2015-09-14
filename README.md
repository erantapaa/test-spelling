A Haskell implementation of Peter Norvig's spelling corrector.

Mainly used to test various hashmap implementations.

Timing the corrector
===

The `spelling-exe` app can be used the time the corrector using different
hashmap implementations. The following implementations are available:

    impl   type       hash                 notes
    -----  -----      -----                ------
    1      ByteString custom               reads words.count; only lowercase
    2      ByteString Data.HashMap.Strict  reads words.count; only lowercase
    3      ByteString Data.HashMap.Strict  reads big.lower; only lowercase
    4      Text       Data.HashMap.Strict  reads from big.txt; handles lowercase
    5      LazyText   Data.HashMap.Strict  reads from big.txt; handles lowercase

Implementations 1-3 assume the training data consist only of lowercase characters.

Here is some timing data for implementation "4" running the same correction for 10, 20 and 30 iterations:

    time spelling-exe 4 howwa 10     # 2.812s
    time spelling-exe 4 howwa 20     # 3.261s
    time spelling-exe 4 howwa 30     # 3.697s

By interpolation, implementation 4 corrects the word "howwa" in about 45 ms.
Also, it takes about 2 secs to read the training data from `big.txt`.

For Python, we can use the `%timeit` facility in ipython:

    $ cd norvig
    $ ipython

    In [1]: from correct import *

    In [2]: %timeit correct("howwa")
    10 loops, best of 3: 49.7 ms per loop

So the two implementations seem comparable. Note that the python code
reads in the training data via `big.txt`, but this parsing time is not
included in the timing of the `correct()` call.

