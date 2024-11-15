# experimental port to CLR

Original copyright © 2013 Zachary Tellman, distributed under the MIT License.

BitSet.cs is Copyright © 1998, 1999, 2000, 2001, 2004, 2005  Free Software Foundation, Inc.  (see file for more information)

BitSet type in immutable_bitset.clj is translated from BitSet.cs

## Status

I can load immutable_bitset.clj and run it with BitSet.cs and it sort of works.

Almost all generative tests pass but for benchmark tests will also need something like Criterium for ClojureCLR.

The failing test case is the empty set but I'm unsure how to deal with the difference between:

```
(.hashCode #{}) ;; 0 on JVM
(.GetHashCode #{}) ;; -15128758 on CLR
```

Not sure if this matters really. Zero hash code for empty collections may just be a best-effort JVM thing?

```
(def generic-hash-set (new |System.Collections.Generic.HashSet`1[System.Object]|))
(.GetHashCode #{}) ;; -15128758
(.GetHashCode generic-hash-set) ;; 13005406
(= #{} #{} generic-hash-set) ;; true
```

Related: https://ask.clojure.org/index.php/1904/inconsistent-hash-with-java-collections

## TODO

Make more REPL friendly. I get strange test failures which can be fixed by full restart:

```
dotnet run &
rep --port 1667 '(load-file "/home/james.davidson/immutable-bitset/src/immutable_bitset.clj") \
 (load-file "/home/james.davidson/immutable-bitset/src/chuck.cljr") \
 (load-file "/home/james.davidson/immutable-bitset/src/collection_check/core.cljc") \
 (load-file "/home/james.davidson/immutable-bitset/test/immutable_bitset_simple_check.clj") \
 (in-ns (symbol "immutable-bitset-simple-check")) (run-tests)'
```

Profile and optimise. Avoid reflection etc. Compare with BitSet.cs

# Project Moved to Contrib!

ztellman/immutable-bitset is now [clojure/data.int-map](https://github.com/clojure/data.int-map)

## Immutable Integer Sets

Clojure's immutable sets are great, but they can be hugely inefficient when we're only trying to store integers.  This library contains an implementation for immutable sets which are both faster and smaller for this special case.

### usage

```clj
[immutable-bitset "0.1.6"]
```

All functions are in the `immutable-bitset` namespace.  There are two constructors, `sparse-bitset` and `dense-bitset`, and three operators, `union`, `intersection`, and `difference`.

```clj
immutable-bitset> (sparse-bitset)
#{}
immutable-bitset> (conj *1 1 2 3)
#{1 2 3}
immutable-bitset> (disj *1 2)
#{1 3}
immutable-bitset> (sparse-bitset [1 2 3])
#{1 2 3}
immutable-bitset> (transient *1)
#<TransientBitSet immutable_bitset.TransientBitSet@bee743d>
immutable-bitset> (disj! *1 2)
#<TransientBitSet immutable_bitset.TransientBitSet@2aa42d42>
immutable-bitset> (conj! *1 5)
#<TransientBitSet immutable_bitset.TransientBitSet@7c079ab0>
immutable-bitset> (persistent! *1)
#{1 3 5}
immutable-bitset> (union *1 (sparse-bitset [5 7 9]))
#{1 3 5 7 9}
```

The set algebra operators take two bitsets, and return a bitset.  Use `clojure.set` for general set operations.

`dense-bitset` behaves the same as `sparse-bitset`, the difference is only in their memory efficiency.  Consider a case where we create a set of all numbers between one and one million:

```clj
(def s (range 1e6))

(into #{} s)              ; ~100mb
(into (sparse-bitset) s)  ; ~1mb
(into (dense-bitset) s)   ; ~150kb
```

Both of these are significantly smaller than the standard set, but the dense bitset is almost an order of magnitude smaller than the sparse variant.  This is because the dense bitset allocates larger contiguous chunks, which is great if the numbers are densely clustered.  However, if the numbers are sparse:

```clj
(def s (map (partial * 1e6) (range 1e6)))

(into #{} s)              ; ~100mb
(into (sparse-bitset) s)  ; ~130mb
(into (dense-bitset) s)   ; ~670mb
```

In this case, the dense bitset is much less efficient than the standard set, while the sparse bitset is about equally large.  So as a rule of thumb, use `dense-bitset` where the elements are densely clustered (each element has multiple elements within +/- 1000), and `sparse-bitset` for everything else.

The bitsets are both somewhat faster than the standard set, as well; adding elements to the transient bitset can be ~50-100% faster than a normal set.

### license

Copyright © 2013 Zachary Tellman

Distributed under the [MIT License](http://opensource.org/licenses/MIT)
