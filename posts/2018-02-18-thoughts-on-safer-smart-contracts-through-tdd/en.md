---
title: Thoughts on Safer Smart Contracts Through Type-Driven Development
---

## Introduction

Last year, I learned of the Idris language and wrote a [blog post](https://harfangk.github.io/2017/10/24/tdd-with-idris-review.html) after reading a book on it. Coincidentally, I joined a company that worked on blockchain technology almost immediately after publishing that post. So when I came across a paper titled ["Safer smart contracts through type-driven development: Using dependent and polymorphic types for safer development of smart contracts"](https://publications.lib.chalmers.se/records/fulltext/234939/234939.pdf), which combined two topics of my interest, I just had to read it.

This post is a collection of the thoughts that came across my mind while I was reading the paper. There won't be any new idea that builds on the contents of the paper. I assume basic understanding of functional programming paradigm, in particular the distinction between pure functions and side effects.

<!--more-->

## Types to Cover More Run-time Computations at Compile Time

In blockchain industry, guaranteeing the correctness of smart contracts is one of the key areas for improvement. As the industry grows, values moved through smart contracts would continue to grow as well, which also means that the damage caused by faulty smart contracts would also increase in proportion. As I learned more and more about smart contracts, I started to think that statically typed functional languages would be great in this domain, since they are well-known for minimizing run-time errors. It is no coincidence that financial institutions use such languages for formal description of contracts. I've also personally experienced how type checking at compile time can help prevent certain kinds of common errors such as null pointer exception.

The authors of the paper, Jack Pettersson and Robert Edström, had the same idea and decided to explore it. Their thesis goes: "Our thesis is that a functional language with an extensive type system can allow safer development of smart contracts. Specifically, we consider three classes of errors that are common due to the characteristics of current smart contract platforms."

After reading through their work, I became more confident that statically typed programming language would be excellent for writing robust smart contracts. Human mind is incredibly powerful and flexible but is also more prone to errors - especially when we are tired or distracted. For reliability, we need to leverage the assistance of computers.

## Types to Represent Pre-conditions and Post-conditions

Here's a snippet from the paper that fascinated me the most:

```haskell
finalize : { auto p: LTE 20 b } -> Eff Int 
           [ STORE, ETH 0 b 0 0 ]
           (\winner => if winner == 0 
                          then [ STORE, ETH 0 b 0 0 ] 
                          else [ STORE, ETH 0 b 20 0 ])
```

It's a type signature for a function called `finalize`, which interprets the final result of a rock-paper-scissors game where each player bets 10 Ethereum and the winner takes it all. This type signature provides a ton of information, and could be hard to understand if you are not familiar with this kind of language.

Here's what each part means:

`{ auto p: LTE 20 b }` part supplies a proof (`p` stands for proof) to the Idris compiler that `20` is less than or equal to `b`, which stands for balance here. So based on the proof, the compiler can assume that `b` is larger than `20` when the function is called. This ensures at compile time that no overdraft would happen.

`EFFECT` is the type Idris uses for describing side effects. You can find a [tutorial](http://docs.idris-lang.org/en/latest/effects/) for `Effects` library at the official Idris website. Note that currently another module is recommended over `Effects`, as stated in the above link: > Unless you have a particular reason to use Effects you are strongly recommended to use `Control.ST` instead.

Here `Eff` is a constructor for `EFFECT` type that takes three arguments: a value of `Int`, a list of `Effect` values, and a function of `Int -> List Effect`. 

The first argument represents the return value of this function. For `finalize`, it's `Int` type. 

The second argument is called `input effects`. It means that this function can access effects `STORE` and `ETH 0 b 0 0` when it is called. 

The third argument is called `output effects`. It is a function that takes the first argument and determines which side effect `finalize` should have between `[ STORE, ETH 0 b 0 0 ]` and `[ STORE, ETH 0 b 20 0 ]`. 

What's significant is that the type signature describes not only the function's output, but also what kind of side effect it should have depending on the result of `finalize`. This gives a powerful tool for explicitly handling side effects, such as network communication, by enforcing their pre-condition side effects and post-condition side effects as types.

## Transforming From One AST to Another AST Is Not Magical

As a self-taught web developer, I lack knowledge in low-level areas such as hardware, assembly, compiler, or operating systems, so those areas still remain mythical "here be dragons" territory to me. So when the writers described the process of transforming the AST output of Idris to the AST of Serpent, a language used in Ethereum Virtual Machine, I was surprised to see how straightforward it was. In principle, I just needed to write derivarion rules for mapping from one AST to another! Of course it would never be that simple, but I felt like I gained a glimpse into the process.

## FP Cannot Represent Message Passing Among Distributed Nodes

In the discussion section, the authors express their disappointment with the functional paradigm:

> When writing smart contracts in our Idris implementation, most of the critical functionality has to exist in effectful rather than pure functions. This is because pure functions have no notion of communication, but Ethereum’s execution model is based entirely on messages that are sent between accounts, which it has in common with all smart contract platforms we are aware of. Furthermore, pure functions don’t directly encode program state, which is another important aspect of smart contract platforms.

Since I've also thought that functional paradigm would be a great fit, I was surprised by the result as the authors were. But they seem to be correct; if a model fails to represent key components of a domain, it might not be the right model. The authors suggest that [process calculus](https://www.wikiwand.com/en/Process_calculus) might be a better fit for this domain. Since I've never heard of it, I had to look it up. It sounded related to the actor model that I got familiar through Erlang and Elixir, and does seem like a better model to formally represent complex network communications.

## Dependently Typed Side Effects Are Incredibly Powerful

The authors note that a combination of dependent types and explicit side effect management of Idris were essential for writing smart contracts in Idris. They wrote a sample implementation as part of the paper, which was quite interesting to read.

Recently I have been working extensively with Elm, and I think I get what the authors meant by the combination being essential. Elm also provides explicit management of side effects through `Cmd` and `Sub` types, but they are not as descriptive as I want them to be. I know that it's in accordance with Elm's design philosophy, and it does make Elm code much easier to approach and understand. But sometimes I do wish that Elm had more robust type system so that I could be more expressive.

For example, let's think about how to express a part of the above snippet in Elm. The goal is: I want to make sure that people cannot send more Ethereum than they own. Elm uses `Cmd Msg` type to describe side effect, where `Msg` type is usually defined as a union type. Let's say it's defined as `type Msg = SendEth Int | DepositEth Int`. If my Ethereum balance is 50, I shouldn't be able to execute `Cmd (SendEth i)` when `i` is greater than 50. Because the value of `i` is determined at run-time, the validity of this side effect can be checked only at run-time.

Idris can check this at compile time thanks to its dependent types. Because `ETH` is defined as a dependent type, `ETH 0 b 0 0` and `ETH 0 b 20 0` in the above snippet are computed as different types as they receive different values for the third argument. And because effects are distinguished at type level, the compiler can guarantee that only valid side effects will be executed. Such guarantee at compile time is impossible in Elm's type system.

## Conclusion

I enjoyed Idris codes in the paper since they showed somewhat realistic example of how to write an Idris program. In particular, it was amazing to see how Idris can be used to specify not just the input and output types, but also the input and output side effects. Idris keeps expanding my horizon about what is possible in programming - I look forward to learning more of it.
