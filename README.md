# Champ-Lite

Sometimes you need a hash table-like data structure that is iteration-safe in the presence
of concurrent writes.  FSet's functional maps have this property; but if all you want to
do is put, get, remove, and iterate, pulling in all of FSet may feel excessive.

So I've extracted just the parts of FSet's
[CHAMP](https://michael.steindorfer.name/publications/phd-thesis-efficient-immutable-collections.pdf)
maps that are needed for this use case into this project.  It has no dependencies and I've
placed it in the public domain, so there should be no barrier to using it, even in a CL
implementation.

There are actually two APIs, one for a persistent functional map, the other for a more
traditional mutable hash table.  The hash table is still updated functionally internally,
so it's still iteration-safe.  Pick and choose the parts you want.  Neither API uses
generic functions.

Here are some reasons you might want to use Champ-Lite instead of CL's built-in hash
tables:

- the functional map API lets you cheaply keep snapshots of past states
- no need to lock readers at all
- no concern about concurrent iterations and updates (iterations see the state as of the
  moment the iteration started)
- single-threaded read performance is close enough to that of CL hash tables
- multithreaded read performance can actually exceed that of CL hash tables in certain
  scenarios 

Reasons against:

- writes are slower: on SBCL, building a 200-entry table takes about 3x as long; for
  updating existing keys, the ratio goes to 10x, not because CHAMP gets slower but because
  SBCL gets faster
- being portable CL code, it can't do `eq` or `eql` hashing in general, although it can if
  the key type is designed to support it; symbols work, as do user classes whose instances
  are assigned serial numbers that are used by the hash function

NOTE: I have added `setf` functionality for both `map-lookup`, which operates on the
persistent functional maps, and `table-get`, which operates on the mutable tables.  Though
they appear similar, they work very differently!  `(setf (map-lookup m x) y)` updates `m`
functionally and writes the new map value back to `m`; no aliases will see the update.
`(setf (table-get tbl x) y)`, on the other hand, mutates `tbl`.  Examples:

```common-lisp
(let ((m (make-map #'sxhash #'equal))
      (tbl (make-table #'sxhash #'equal)))
  (setf (map-lookup m 'key) 'old-value)
  (setf (table-get tbl 'key) 'old-value)
  (let ((m-copy m)
        (tbl-alias tbl))
    (setf (map-lookup m 'key) 'new-value)
    (setf (table-get tbl 'key) 'new-value)
    (print (map-lookup m-copy 'key))      ; --> OLD-VALUE
    (print (table-get tbl-alias 'key))    ; --> NEW-VALUE
    ...))
```

Semantically, you can think of it as if the binding of `m-copy` made a copy, though that
isn't actually what's happening; the difference is in the `setf` expander for
`map-lookup`.
