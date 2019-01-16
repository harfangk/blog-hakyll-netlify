---
title: Common Misconceptions of Beginners About Type System
---
One of the best tips I got about writing a blog post is to write posts that I would have wanted to read a year ago. This is one of them. I hope this would clarify misconceptions commonly held by beginners about type system.

<!--more-->

## Why do we need a type?

One of the first things one encounters in a programming language is its types. Types like integer, float, or string can be seen in almost all languages. But why do we need types?

Remember that everything on computer is processed in bits after all. For example, a megabyte of something is represented by 8388608 (2 ^ 23) 0s and 1s in it. It's almost impossible for humans to figure out what those bits stand for, but it's also not easy for computers to do it efficiently without help. 

Type system allows both humans and computers to easily discern meaningful patterns in the sea of bits by imposing certain rules about how to read and write chunks of bits. And these chunks of bits following certain rules are called types. 

Let's try an analogy to lego bricks. Lego bricks have certain shapes according to rules like 2 x 4, 1 x 4, 2 x 2. Those are the types of lego bricks. Imagine if you are only given 1 x 1 bricks that are building blocks of those brick types instead. It'd be still possible to build a complicated structure with them, but that would take much more manual and mental work to do so than when building with larger, standardized bricks.

Type system does not come for free. Both humans and computers have to know and learn about the rules used by the particular type system they are using. They also need to follow those rules, or the program will refuse to work. Still, the benefits outweigh the costs so type systems are used in all languages.

With that said, I'd like to address some common misunderstanding about type system.

## Forget about strong and weak

Strong and weak is one of the most commonly used classifications of type system. Unfortunately, there's no official definition of strong and weak type systems, so each person has his or her own definition of it. Lacking any common ground, all discussions about strong and weak type eventually degenerate into pointless emotional arguments.

Don't pay attention to any discussion involving strong or weak type systems. When someone says a type system is strongly typed, understand it as: I like this type system and feel comfortable with it. Likewise, when someone says something is weak, understand it as: I feel worried or uncomfortable using this system.

## Stop thinking in dichotomy

Type systems are often discussed in terms of opposites. For example, static vs dynamic type system is a widely  known distinction. This way of talking about type system, however, misleads people into thinking in a strict dichotomy. 

In reality, static and dynamic type systems are not mutually exclusive. They are two different approaches to the same problem with different pros and cons. All programming languages use elements from both of them. 

It's better to think of static type system as a system where static approach is more dominant than dynamic approach, and vice versa for dynamic type system.

This is not limited to static and dynamic distinction. Other smaller distinctions of type systems also tend to be not mutually exclusive, or are better understood as a spectrum rather than a dichotomy.

## Static type system does not require type declaration

If you were exposed to static type system through C and Java like me, you might have this misunderstanding. Java requires you to explicitly declare the type of every single variable. This is something called explicit type declaration. But not all static type systems use explicit declaration.

In static type systems with type inference, compilers automatically infer the type of variables by analyzing the source code. This can be seen in languages like Swift, Haskell, Go, and so on. Type inference is so great that once you've tried it, you'd never want to go back to statically typed languages without it.
