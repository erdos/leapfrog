# Leapfrog Triejoin in Clojure

This is a functional, immutable implementation of the [Leapfrog Triejoin](https://arxiv.org/abs/1210.0481) algorithm in Clojure.

## Motivation

Most implementations of LFTJ are inherently imperative.
The paper is a great read but I needed a version I couldn read and reason about as I worked.

Because the iterators are immutable, there is no need to implement `atEnd()` or `up()`, and the data structures fit nicely into the existing ecosystem. (Sequable, IReduceInit, etc.)

Current implementation is tested, documented, aimed to be simple to read and reason about. However, the implementation is also naive, allocation-heavy and the constant factors to the runtime are definitely suboptimal. Optimization for the above was a non-goal, so always choose the right implementation strategy for your use case.


## Usage

The artifacts are not yet published, but you can use it with `deps.edn` as a [git dependency](https://clojure.org/reference/deps_edn#deps_git_sha).

This library defines two protocols: `LinearIterator` and `TrieIterator` to define the data the algorithm can work on.

### Linear Iterators

Utility function `sorted-iter` can be used to produce a `LinearIterator` from an already sorted collection.

```clojure
(def iter-1 (sorted-iter (sorted-set 1 2 3 4)))
(def iter-2 (sorted-iter (sorted-set 0 2 4 6 8)))
```

Then, function `leapfrog-join` produces a new `LinearIterator` to visit elements present in both collections.

```clojure
(leapfrog-join [iter-1 iter-2]) ;=> [2 4]
```

### Trie Iterator

Utility function `trie-iterator` can be used to produce a `TrieIterator` from a set of tuples.
However, most functions also need a *variable ordering* to work with, so most functions will work with
maps of keys `:variable-ordering` providing a vector of variables and `:trie-iterator` holding a `TrieIterator`.

```clojure
(def trie1 (trie-iterator [[1 2 3] [1 2 4] ...]))
(def t1 {:variable-ordering [:a :b :c] :trie-iterator trie1})
```

- Function `union` constructs the iterator of the union of multiple such trees.
- Function `trie-join` constructs the intersection of such trees.
- Function `trie-antijoin` constructs the antijoin (difference) of two trees.

Most functions have a `-iterator` suffixed version also which directly returns a `TrieIterator` instead of the map form.
There are also some helpers available when working with tries:

- Functions `bfs` and `dfs` return a breadth-first and a depth-first walk over the supplied trie.
- Function `trie-routes` returns a lazy seq of all paths from the root to the leaves of the trie.
- Function `reorder` can be used to change the variable ordering of a trie iterator.
- Function `eager` returns a view of a trie iterator that skips incomplete branches.

See `triejoin_test.clj` for usage examples.

## Development

Contributions are welcome. Feel free to reach out by opening an Issue here or directly on Clojurians Slack or LinkedIn.
When working on this repo, you can:

- Run tests with `clojure -M:test`.
- Generate API docs in Markdown: `clojure -T:build doc`.
- Try to improve test coverage.
- Add documentation.

## License

Copyright (c) Janos Erdos. All rights reserved. The use and distribution terms for this software are covered by the Eclipse Public License 2.0 (https://www.eclipse.org/legal/epl-2.0/) which can be found in the file LICENSE.txt at the root of this distribution. By using this software in any fashion, you are agreeing to be bound by the terms of this license. You must not remove this notice, or any other, from this software.