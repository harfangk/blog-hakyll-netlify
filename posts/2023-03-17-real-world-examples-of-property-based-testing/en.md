---
title: Real World Examples of Property-based Testing
---

Property-based testing is a great tool, but it’s sadly underutilized. I believe
that it’s superior to unit testing and should be used over unit testing
whenever possible. If you are not familiar with it, you should google it.
These days all major languages have a library for it, so just look for a resource
that uses your favorite language to introduce it. Nevertheless, for the most
in-depth introduction I recommend the following videos:

- [Testing Asynchronous APIs With Quickcheck](https://youtu.be/iW2J7Of8jsE) by
  Thomas Arts
  ([slides](http://www.erlang-factory.com/static/upload/media/1461230674757746pbterlangfactorypptx.pdf))
- [Don’t Write Tests](https://youtu.be/hXnS_Xjwk2Y) by John Hughes

<!--more-->

Unfortunately, property-based tests are much harder to write than unit tests.
Writing tests for a property of the program requires that you understand the
said property, and express it without using the implementation of the function
being tested. Sometimes it even feels like solving a brain teaser. Here’s one
of the most commonly given out examples: how can I write a test that a function
that reverses a list works correctly? The answer: reversing a list twice should
return the original list. Just like brain teasers, writing property-based test
becomes easier the more examples you see.

So I’d like to share some property-based tests I wrote for my big number
library for Elm language. It was the perfect fit for property-based tests, as
there are existing mathematical properties I can test for, and unit tests, even
dozens or hundreds of them, don’t provide enough correctness for this kind of
library.

```elm 
decDecimalFuzzer : Fuzzer Decimal
decDecimalFuzzer =
    let
        int =
            Fuzz.intRange 1 5
                |> Fuzz.andThen (\i -> List.repeat i (Fuzz.uniformInt Random.maxInt) |> Fuzz.sequence)
                |> Fuzz.map (List.map String.fromInt >> List.foldl (++) "")

        fraction =
            Fuzz.intRange 0 3
                |> Fuzz.andThen (\i -> List.repeat i (Fuzz.uniformInt Random.maxInt) |> Fuzz.sequence)
                |> Fuzz.map (List.map String.fromInt >> List.foldl (++) "")

        sign =
            Fuzz.oneOfValues [ "", "-" ]
    in
    Fuzz.map2 (++) (Fuzz.constant ".") fraction
        |> Fuzz.map2 (++) int
        |> Fuzz.map2 (++) sign
        |> Fuzz.map (Decimal.fromString >> Maybe.withDefault (Decimal.fromInt 0))
```

Here’s the custom generator for a big decimal. It generates string
representations of big decimal numbers, then turn them into the `Decimal` type
that the library uses. For the string representation, it follows this process:

1. Generate the integer part by concatenating 1 to 5 random number between 0
and 2^32 - 1. 
2. Generate the fraction part by concatenating 0 to 3 random number between 0 and 2 ^32 - 1. 
3. Randomly choose between positive and negative sign. 
4. Concatenate them all, and parse them into `Decimal` value.

```elm 
describe "negate"
    [ Test.fuzz fuzzer "should return original i when applied twice" <|
        \i ->
            let
                i_ =
                    Integer.negate << Integer.negate <| i
            in
            Expect.equal i i_
    ]
```

This is the test for negate function. Just like reversing a list, it relies on
the fact that negating a number twice should return the original number.

```elm 
describe "fromString and toString"
    [ Test.fuzz fuzzer "should be inverse functions" <|
        \i -> Expect.equal (Integer.fromString (Integer.toString i)) (Just i)
    ]
```

This is the test for fromString and toString function. It relies on the fact
that these two functions are inverse of each other.

```elm 
describe "add"
  [ Test.fuzz2 fuzzer fuzzer "should have transitivity property" <|
      \i1 i2 ->
          Expect.equal (Integer.add i1 i2) (Integer.add i2 i1)
  , Test.fuzz3 fuzzer fuzzer fuzzer "should have associativity property" <|
      \i1 i2 i3 ->
          Expect.equal (Integer.add (Integer.add i1 i2) i3) (Integer.add i1 (Integer.add i2 i3))
  , Test.fuzz fuzzer "should have identity property" <|
      \i ->
          Expect.equal (Integer.add Integer.zero i) i
  , Test.fuzz fuzzer "should return zero for addition with negative self" <|
      \i ->
          Expect.equal (Integer.add i (Integer.negate i)) Integer.zero
  , Test.fuzz2 Fuzz.int Fuzz.int "should have same result for addition as Int" <|
      \i1 i2 ->
          Expect.equal (Integer.add (Integer.fromInt i1) (Integer.fromInt i2)) (Integer.fromInt (i1 + i2))
  ]
```

This is the test for `add` function. It tests associativity, transitivity, and
identity properties of numbers. It also tests that adding self with negative
self returns zero. Note that the last test relies on Elm’s `Basic` library to
test that addition produces correct result at least for `Integer` values within
Javascript’s integer range. 

```elm 
describe "mul"
    [ fuzz2 fuzzer fuzzer "should have transitivity property" <|
        \d1 d2 ->
            withinTolerance (Decimal.mul d1 d2) (Decimal.mul d2 d1)
    , fuzz3 fuzzer fuzzer fuzzer "should have associativity property" <|
        \d1 d2 d3 ->
            withinTolerance
                (Decimal.mulToMinE (Decimal.minExponent * 2) (Decimal.mulToMinE (Decimal.minExponent * 2) d1 d2) d3)
                (Decimal.mulToMinE (Decimal.minExponent * 2) d1 (Decimal.mulToMinE (Decimal.minExponent * 2) d2 d3))
    , fuzz fuzzer "should have identity property" <|
        \d ->
            withinTolerance (Decimal.mul (Decimal.fromFloat 1) d) d
    , fuzz3 fuzzer fuzzer fuzzer "should have distributive property" <|
        \d1 d2 d3 ->
            withinTolerance
                (Decimal.mulToMinE (Decimal.minExponent * 2) d1 (Decimal.add d2 d3))
                (Decimal.add (Decimal.mulToMinE (Decimal.minExponent * 2) d1 d2) (Decimal.mulToMinE (Decimal.minExponent * 2) d1 d3))
    ]
```

This is the test for `mul` function for `Decimal` type. It follows similar
pattern by using inverse functions, but now the stake is higher than `Integer`
because `Decimal` is a much more complex data type. I would have been fine, if
slightly uneasy, about correctness of `Integer` type with unit tests. I’m
familiar with the usual suspects for that type - 0, NaN, positive and negative
infinity, sufficiently long numbers, and so on.

`Decimal` is a different beast. I was not familiar with edge cases for
`Decimal` at the time of writing the library, and there were simply too many
combinations that I would have to test to be even moderately confident with the
correctness of the program. And property-based tests helped me find several
bugs for `mul`, `div`, and `sqrt` functions that I would have been unable to
discover with unit tests.

And that’s it for the examples. You can look at more tests in my library, but
there isn’t any new technique there. I hope this post was helpful in getting
started with writing property-based tests. 
