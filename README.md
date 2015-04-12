Godel Hashes for Erlang.
=====

An OTP library implementing Shuying Liang et al.'s paper describing
Godel Hashes. The paper is available: http://matt.might.net/papers/liang2014godel.pdf

The library provides the gset module, which mostly implements the
erlang sets interface, but leaves out some stuff which is intractable
(although only notionally, they could be implemented but they would
scale quite poorly).

To use the library, one must first instantiate a Universe of
Discourse, which establishes the prime mapping for the sets.

Examples
--------

```

```

Build
-----

    $ rebar3 compile
