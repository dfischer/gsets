Gödel Hashes for Erlang.
=====

An OTP library implementing Shuying Liang et al.'s paper describing
Godel Hashes. The paper is available here: http://matt.might.net/papers/liang2014godel.pdf

The library provides the gset module, which mostly implements the
erlang sets interface, but leaves out some stuff which is intractable
(although only notionally, they could be implemented but they would
scale quite poorly).

To use the library, one must first instantiate a Universe of
Discourse, which establishes the prime mapping for the sets.  All comparable sets must share the same universe of discourse.

The point of Gödel hashes is that for certain operations, notable intersection, subset/subsumption, and unions, they're much faster than the set types in the standard library[citation needed].

Examples
--------

```



```

Build
-----

    $ rebar3 compile

Test
----

    $ rebar3 proper
    
Use
----

To use the library, add it to your project's rebar.config as a rebar3 dep:

```
{deps, [gsets]}.
```

or 

```
{deps, [{gsets, "0.1.0"]}.
```
