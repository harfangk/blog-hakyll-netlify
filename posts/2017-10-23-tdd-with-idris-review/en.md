---
title: Type-Driven Development with Idris - Review
---

## Summary

[Type-Driven Development with Idris](https://www.manning.com/books/type-driven-development-with-idris) introduces a software development approach that uses types as the primary tool for building softwares. In this paradigm, types are given much more responsibilities than their traditional role of checking data validity. For example, types can be used to represent the input and output states of functions or various contracts that functions and datas must fulfill. Such descriptions and contracts are enforced at compile time, providing much stronger guarantee of software correctness at compile time than other approaches can.

Idris programming language is used in the book to teach type-driven development (TDD). Idris is a general purpose pure functional programming language created by Edwin Brady, who is also the author of this book. Idris is inspired by Haskell and ML and sports a state of the art type system suited for TDD. You will learn basics of Idris language alongside TDD in this book.

<!--more-->

## Who Is It For

I recommend this book to those who are interested in using statically typed system to build programs with strong guarantee of correctness. You should be familiar with basic concepts of functional programming such as immutability, closure, or higher-order function. It would be very helpful if you are already familiar with functional programming languages like Haskell, OCaml, or Scala. The book introduces a lot of new concepts, so you'd rather avoid having to learn about functors while trying to understand those other new concepts introduced in the book.

I do not recommend this book if you are not interested in statically typed system or if you are a pragmatist interested in concepts and languages that can be used in production right away. In addition, if you have little experience with functional programming, this book might feel overwhelming because of the sheer amount of prerequisite knowledge.

## What Makes It Special

What I appreciate the most is that the author maintains clear focus on his stated goal of introducing TDD. Far too many books, especially the ones that deal with topics that their authors are passionate about, have unncessary chapters that digress from the topic of the book. In contrast, this entire book is written and organized for the introduction of TDD with a clear sense of progression between each chapter. The author begins by introducing basic concepts and setting up development environment; then he provides increasingly advanced examples of TDD; in the final five chapters, he presents somewhat more realistic examples like file IO or concurrent programming.

Another thing I like about this book is that the author teaches the concepts systematically, which is a somewhat rare skill among technical writers. At the beginning of the book, he introduces an iterative process for TDD: "type, define, refine: writing a type, implementing a function to satisfy that type, and refining the type or definition as we learn more about the problem." This process is used throughout the book, each time in a slightly different context to demonstrate its application. Such repetition of identical process under different circumstances is one of the most effective method for teaching.

The author also introduces new concepts through tutorial-like examples. He first provides some code to serve as a starting point, and then guides the readers through each step, all the while adhering to the "type, define, refine" process. Here is a simple example with step-by-step progression from Chapter 3:

```haskell
-- For example, to write the exclusive OR operator, you could follow these steps:

-- 1. Type — Start by giving a type:
xor : Bool -> Bool -> Bool

-- 2. Define — Press Ctrl-Alt-A with the cursor over xor to add a skeleton definition:
xor : Bool -> Bool -> Bool
xor x y = ?xor_rhs

-- 3. Define—Press Ctrl-Alt-C over the x to give the two possible cases for x :
xor : Bool -> Bool -> Bool
xor False y = ?xor_rhs_1
xor True y = ?xor_rhs_2

-- 4. Refine—Complete the definition by filling in the right sides:
xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y
```

Lastly, the author also provides nice exercises for each chapter. Whereas some books provide difficult exercises that can only be solved when you thoroughly understand new concepts, the exercises in this book are relatively simple ones aimed at getting you more familiar with them. I think that this kind of exercises is more appropriate when you are trying to learn something on your own through books.

## Type-Driven Development (TDD)

In functional programming paradigm, a program is seen as a process of transforming data from one form to another. Such data forms or structures are represented as types, and the transformation process is represented as a function with input and output. TDD puts emphasis on using types and functions to depict the overall plan and structure of a program. The main process in TDD is "type, define, refine", as previously mentioned.

As its name suggests, in TDD we write down types first. These types are used to explicitly represent the conceptual model of the program that we intend to write. Let's say we want to represent the states of matter. We could start by defining `data Matter = Solid | Liquid | Gas`. Then we can represent a phase transition from one state to another as a function `phaseTransition : Matter -> Matter`.

Once we define initial types and functions, we keep refining these definitions so that the software would more precisely match our conceptual model. Let's say we'd like to add the plasma state to our program. Then we can refine our type like `data Matter = Solid | Liquid | Gas | Plasma`. We repeat this "type, define, refine" process until we reach a satisfactory iteration.

I believe that rather than being a completely novel approach, TDD is more of an organized description of a software development approach that organically arose in communities that use statically typed functional languages like Haskell. Whenever I read or write Haskell programs, I find myself focusing foremost on type definitions and function type signatures to gain overall understanding of the program. TDD felt like an improved and formalized version of such an approach.

## Idris Programming Language

Having said that, TDD as presented in this book is way more powerful than what little I've seen in Haskell. It is because of additional features offered by Idris programming language. Let's look at some of its notable characteristics.

### First-class Type

Idris has first-class type and can pass around types as arguments to other functions. If you've experienced the paradigm shift that occurred when first-class function was introduced, you could guess how much this changes the game. Like first-class function, first-class type opens up a completely new way to express our intents through code.

As a simple example of first-class type, in Idris `List` type is defined as `data List : (elem : Type) -> Type`. It's just a regular type that takes another type as an argument, without special interface-like syntax provided by the language. Unfortunately, the full implications of first-class type is hard to demonstrate through such trivial example. In Idris, this feature is used mostly in conjunction with dependent type, which we will look at next.

### Dependent Types

Dependent type is one of the defining features of Idris. It allows us to define types that can be calculated from other values. Personally, I found it easiest to think about it as a function at type level.

For example, we can think of `Vector 4 String` type, which represents a list of `String` with exactly 4 elements. We can keep creating similar types like `Vector 0 String`, `Vector 1 String`, `Vector 2 String`, and so on by entering different values as their lengths. This can be generalized as `Vector n String` type, which is essentially a function that takes `n` as input and returns types as outputs. The type of output returned by this function is "dependent" on the value of `n`, so this kind of function is called a dependent function. The type of this function, in turn, is called dependent function type or shortened to dependent type.

Dependent type gives us a new dimension of code expressiveness, enabling us to incorporate more information about program into the type system. I found two use cases most interesting. First, information about data that was available only at runtime can be made available at compile time. For example, `Integer` type tells us that a value of its type will be an integer. But whether a variable of that type would be a positive or negative number, or zero can only be determined at runtime. With dependent type, we can define more precise types such as "positive integers that are multiples of three" or "odd integers greater than -50 and less than 9953". We've already seen the `Vect n String` type, which represents "lists of n String items". Because such information is guaranteed by the type system at compile time, we never have to worry about unexpected values.

Second, description about what constitutes a valid program can be represented in the type system. For example, in the context of bank transactions, we can define `Withdrawl` dependent type, which takes not just the amount of money to withdraw, but also "valid credentials" and "bank account with balance greater than withdrawl amount" as arguments. Assuming that we defined the `Withdrawl` type properly, any value of `Withdrawl` type would always represent a valid bank transaction.

Traditionally, such information has been described in separate documents such as UML, comments of the source file, or worse, in the head of the developer. In Idris, it can be directly represented as part of the source code and made available to the compiler through dependent types, ensuring that a program's description and implementation can programatically refer to each other. This means that any change to software requirements will be immediately reflected in the description of the program, which the compiler will use to check whether rest of the program follows the new requirements. There would be no more need to continuously go back and forth between documents and source code to ensure that new requirements are reflected.

In the book, the author demonstrates several use cases of dependent type. I'll extend the state of matter example to showcase how to represent input and output states of operations in a dependent type.

```haskell
data Matter = Solid | Liquid | Gas

data PhaseTransition : Type -> Matter -> Matter -> Type where
    Melt : PhaseTransition () Solid Liquid
    Vaporize : PhaseTransition () Liquid Gas
    Condense : PhaseTransition () Gas Liquid
    Freeze : PhaseTransition () Liquid Solid
    Sublime : PhaseTransition () Solid Gas
    Deposit : PhaseTransition () Gas Solid

    Pure : ty -> PhaseTransition ty state state
    (>>=) : PhaseTransition a state1 state2 ->
            (a -> PhaseTransition b state2 state3) ->
            PhaseTransition b state1 state3
```

`PhaseTransition` dependent type describes potential phase transitions among states of matter, where last two arguments refer to input and output states. For example, a value of `PhaseTransition` dependent type constructed with `Melt` constructor requires that input and output states must be `Solid` and `Liquid`, respectively. We can use these operations to write a sequence of phase transitions like following:

```haskell
vaporizeIce : PhaseTransition () Solid Gas
vaporizeIce = do Melt
                 Vaporize

vaporizeIceThenFreezeAgain : PhaseTransition () Solid Gas
vaporizeIceThenFreezeAgain = do Melt
                                Vaporize
                                Deposit

freezeSteam : PhaseTransition () Gas Solid
freezeSteam = do Freeze
                 Freeze
```

We can see that the last function does not make sense. To make a gas into solid, we need to condense and then freeze it. Instead, `freezeSteam` function conducts freeze operation twice. Compiler will also notice that the sequence of phase transitions in `freezeSteam` is invalid and and report an error at compile time, which is made possible through dependent type.

### Powerful REPL

Idris has one of the most powerful REPLs I have seen. Let's take a look at the example I've already given above.

```haskell
-- For example, to write the exclusive OR operator, you could follow these
-- steps:

-- 1. Type — Start by giving a type:
xor : Bool -> Bool -> Bool

-- 2. Define — Press Ctrl-Alt-A with the cursor over xor to add a skeleton definition:
xor : Bool -> Bool -> Bool
xor x y = ?xor_rhs

-- 3. Define—Press Ctrl-Alt-C over the x to give the two possible cases for x :
xor : Bool -> Bool -> Bool
xor False y = ?xor_rhs_1
xor True y = ?xor_rhs_2

-- 4. Refine—Complete the definition by filling in the right sides:
xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y
```

Something that starts with `?`, in this case `?xor_rhs`, `?xor_rhs_1`, and `?xor_rhs_2`, is a hole. It stands in for incomplete parts of the program, and Idris REPL provides amazing commands to work with holes. The instructions in the above code refer to some basic commands, such as automatic case splitting or displaying which type the hole stands for. In the following gif, you can see me actually following these instructions. Working in Idris REPL is an incredibly pleasant experience. It feels like having a meticulous and helpful assistant, not a nagging manager.

![terminal gif](/images/idris_repl_demo.gif)

## Other Musings

Overall reading the book was an eye-opening experience. This was by far the most difficult programming language book that I've read, as it introduced a lot of unique concepts. For example, I was dumbstruck when I learned that I had to convince the Idris compiler that `1 + k` and `k + 1` are equivalent types to pass the type check. I've never seen anything like that in other languages.

Idris is a visionary language, which tries to address some inherent flaws of current models of programming. I like it as a language, but I doubt it will ever become a mainstream one. First of all, it requires even more background knowledge than Haskell to use it. On the other hand, its type system is too powerful for typical tasks. Mainstream is all about being "just good enough", but Idris is more about "above and beyond". For typical shopping mall inventory management system, Idris is a bit overqualified. Moreover, I have a hunch that in collaborative environment it would be hamstrung by its excellent expressiveness. There would be so many ways to express something that each team member would end up writing programs in different styles, which causes significant problems for collaboration. There's a reason that Google keeps Go language so plain despite continued criticisms.

Having said that, if I had to write software for nuclear reactors, I would definitely pick Idris, because it gives me unparalleled confidence in my code for such a critical system.
