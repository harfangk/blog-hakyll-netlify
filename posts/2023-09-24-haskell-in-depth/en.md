---
title: 'Review: Haskell in Depth'
---

# My History with Haskell

I have been fascinated with Haskell for years. I started dabbling in it when I worked with Elm several years ago and fell in love with type-oriented programming. Since then, I have studied various introductory materials such as [UPenn CIS 194](https://www.cis.upenn.edu/~cis1940/spring13/lectures.html), [Learn You a Haskell](http://learnyouahaskell.com/), as well as a dozen blog posts and articles. Recently, I also read [Get Programming with Haskell](https://www.manning.com/books/get-programming-with-haskell) to refresh my Haskell knowledge.

<!--more-->

Learning the basics of Haskell was challenging, but even after gaining confidence, I found it difficult to write non-trivial Haskell programs. For instance, implementing algorithms in Haskell was much more challenging than I had expected. Many algorithms required the use of mutable arrays, which is not covered in basic Haskell. The [Array package](https://hackage.haskell.org/package/array-0.5.6.0) was particularly daunting, as it provides four different interfaces consisting of mutable and immutable arrays that can be either strict or lazy. I had to use the mutable unboxed array based on the state monad, but I struggled to fully understand it before using it. Eventually, I gave up on fully understanding it and focused on learning how to use the library, which allowed me to just get done with implementing algorithms.

![](/images/confused.jpg)

Since then, I have read several intermediate-level articles on Haskell on and off. However, most of them felt like they were written by brilliant and well-meaning professors who are terrible at teaching. For example, when I searched for "What is RankNTypes in Haskell," the first result was the [HaskellWiki page on RankNType](https://wiki.haskell.org/Rank-N_types). It provides a brief definition that assumes the reader already knows what [universal quantification](https://www.wikiwand.com/en/Universal_quantification) is. It also includes longer sections on Church-encoded lists and RankNType’s relation to existentials, which are yet more new topics. Other materials tend to have similar pedagogical shortcomings, such as providing concise academic definitions but no examples, introducing multiple new concepts to answer the original question, or going off on a tangent without properly explaining the concept.

![](/images/monad.jpg)

After deciding to go beyond basic Haskell, I began searching for intermediate-level learning materials. I was concerned that there might not be good resources available, but the Haskell community has been getting better in organizing resources over the years. When I first delved into Haskell, there was a lot of discussion about the lack of a good, widely accepted introductory material. However, nowadays [Haskell Programming from First Principles (HPFP)](https://haskellbook.com/) has emerged as the standard introductory book. I hoped to find a similar trend in intermediate-level materials.

# Choosing the Intermediate Haskell Book

I was pleasantly surprised to discover several recently published intermediate-level books on Haskell:

- [Haskell in Depth](https://www.manning.com/books/haskell-in-depth) (May 2021)
- [Functional Design and Architecture](https://www.manning.com/books/functional-design-and-architecture) (December 2021)
- [Production Haskell](https://leanpub.com/production-haskell) (February 2023)
- [Practical Haskell: A Real-World Guide to Functional Programming](https://link.springer.com/book/10.1007/978-1-4842-8581-7) (January 2019)

I had several criteria for selecting a book. Firstly, it should cover commonly used techniques and idioms, including state monads, monad transformers, advanced IO handling, and proficient usage of type classes. Secondly, it should focus on practical aspects of Haskell, such as testing, error handling, and other topics, while minimizing theoretical discussions. Lastly, it should provide numerous code examples with appropriate complexity, as I've had negative experiences with articles that present overly complex examples.

After reviewing the introductions and table of contents, I decided to go with "Haskell in Depth". I skipped "Production Haskell" because it had a section on team-building that didn't immediately interest me. "Practical Haskell" lacked a substantial introduction, so I wasn't sure what to expect. "Functional Design and Architecture" seemed to prioritize architecture over language usage, making it a better choice for later exploration.

# Overview

Overall, the book is a well-written textbook. The sentences are easy to read, using vocabulary commonly found in engineering blog posts rather than in academic papers. The page layout is visually appealing, reflecting the high quality work of the editors at Manning Publications. The progression of topics and pace of explanation are appropriate. The author, Vitaly Bragilevsky, has over 20 years of teaching experience in universities, and his expertise is evident in the book's quality. In the preface, he lays out his practical approach to the topics covered in this book:

> Two unfortunate myths contribute a lot to its limited adoption:
- It is hopeless to program in Haskell without a PhD in math.
- Haskell is not ready/suitable for production.
I believe that both of these claims are false. In fact, we can use Haskell in production
without learning and doing math by ourselves.
…
The truth is, we can apply those mathematical concepts to our code without worrying too much about them. Math is good for applying; it was created and developed over the centuries precisely for that. Nobody bothers about prime numbers and the problem of factorization when buying something with a credit card nowadays.
> 

The book is 600 pages long and consists of 16 chapters, covering a wide range of topics. It starts with an introduction to basic Haskell features and then delves into structuring applications, organizing projects, error handling, testing, profiling, and extensions for advanced type-level programming. It also explores metaprogramming and demonstrates how to use popular libraries for data streaming, concurrency, and database interaction. Additionally, the book includes practical topics such as testing, profiling, and error handling.

# What I Liked

There are several parts of the book that I really liked.

First, I liked the occasional comments on Haskell's language warts. Instead of using the default `String` type, which is a list of `Char` type and can be slow for serious text processing, the book recommends using `Data.Text` and `Data.ByteString`. The Prelude module exposes many unsafe functions and types, such as `head` that crashes when used on empty lists. To avoid potential issues, it is often better to disable the Prelude module with the `NoImplicitPrelude` language extension and use alternative custom preludes. You can learn about more language warts from a series of blog posts [here](https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1/).

Second, I enjoyed the author's coverage of commonly used techniques and idioms. The book provides numerous examples demonstrating the use of basic type classes like `Eq`, `Enum`, `Bounded`, `Show`, `Semigroup`, and `Monoid`. These examples showed me alternative ways to write functions using these type classes, allowing me to become more familiar with them and use them in my own code.

```haskell
-- With monadic binding
locateByName :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName pnumbers locs name =
  lookup name pnumbers >>= flip lookup locs

-- Without monadic binding
locateByName' :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName' pnumbers locs name =
  case lookup name pnumbers of
    Just number -> lookup number locs
    Nothing -> Nothing

-- With fold
rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl (flip rotate)

-- With mconcat
rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' dir ts = rotate (mconcat ts) dir
```

Using Reader monad to implement read-only access to application-wide configuration would have been very useful to know when I was working with Elm years ago. I remember how much of a slog it was to pass the boolean config value for light vs dark mode through multiple layers of functions to just reach the ones responsible for rendering the UI.

Monad transformers are often mentioned in production Haskell, but they were difficult to understand. Although I couldn't fully grasp them just from reading the book, studying multiple examples by a single author helped me understand them much better than reading disjointed blog posts by different authors. With some practice exercises, I should be able to use them confidently.

Third, I found the discussion on how Haskell handles common software engineering practices such as testing, error handling, profiling, and organizing the build process and file structure valuable. While these practices are not particularly different in Haskell compared to other languages, it can be time-consuming to determine the best practices and most commonly used libraries for each task. The book provides clear answers on these topics.

Fourth, I also appreciated the in-depth explanation of how GHC Haskell uses memory at runtime. I learned that GHC uses closures as the main unit of memory usage in the heap, and that they can represent unevaluated thunks, fully evaluated normal forms, or partially evaluated weak head normal forms, each with a different memory footprint. I also corrected my misunderstanding of `seq`, a function that forces evaluation in Haskell. Instead of fully evaluating the expression as I had thought, it stops after evaluating to the weak head normal form.

Fifth, the explanation of Haskell metaprogramming was also enlightening. Although I found data-type-generic metaprogramming confusing, its usage of abstract syntactic trees reminded me of how metaprogramming works in Elixir. Template Haskell, on the other hand, was much more complex, and the author provided plenty of warnings about its fragility when it comes to GHC version changes. This echoed many other warnings against using it in production.

Sixth, the book briefly touched on what is possible with advanced type-level programming in Haskell. Chapters 11 and 13 provided a brief overview of various language extensions, including `DataKinds`, `PolyKinds`, `TypeFamilies`, `ScopedTypeVariables`, `KindSignatures`, `TypeApplications`, `TypeOperators`, `AllowAmbiguousTypes`, `ExplicitForAll`, `GADTs`, and `GADTSyntax`. In one way or another, they all allow extending, manipulating, and dictating the types themselves. Although it was impossible to fully understand all of them on the first pass, it gave me a direction to explore if I wanted to delve into heavy type-level programming.

# What I Found Lacking

Nevertheless, there are some parts of the book that I did not like. First, there is a lack of exercise problems. Although the author does a great job explaining the presented codes, passive learning is too ineffective at solidifying the lessons learned compared to active learning. Personally, I took an alternative approach by typing all the source code presented by the author while thinking about how I would implement rest of the code. However, it's not the same as solving carefully curated exercise problems.

Second, the folder structure of the source code is a bit confusing. Codes used in a single chapter are placed in folders like `/ch01` or `/ch13`, while those used across multiple chapters are placed in `/stockquotes` or `/ip`. Interestingly, they are all listed as separate executables and internal libraries within a single Haskell project. I’m not sure if it was intentional, but it was an interesting example that shows how to organize a complex Haskell project.

Third, the example code is somewhat outdated in terms of tooling and GHC version. For installing GHC, the author refers to the Haskell Platform, which was deprecated in 2022. The latest tool is [GHCUP](https://www.haskell.org/ghcup/). Additionally, the source code is targeted at GHC 8.6, released in 2019, while the current stable release is 9.2.8 and 9.4.5. I used GHC 9.4.5 for easier use with recent versions of Haskell Language Server, but had to tinker with versions of some dependencies to get the code compiled. This also gave me a brief exposure to how fragile Template Haskell is to GHC version updates and why it's not recommended for production usage.

Fourth, the pacing for the three chapters in Part 4: Advanced Haskell was noticeably faster compared to other chapters. Although the author does state that the goal is to give a brief overview the features, the difficulty and density of the covered concepts make it hard to keep pace. Dropping Chapter 13 on dependent types, of which adoption seems to have [lost momentum](https://old.reddit.com/r/haskell/comments/13l2cmb/is_dependent_haskell_still_a_thing/) in Haskell community since the author started writing the book in 2019, could make more room for other stable features covered in Chapter 11. Moreover, [Idris language](https://www.idris-lang.org/) provides a more natural experience for learning about dependent types.

# Conclusion

Overall, the book was enjoyable to work with. It provides a cohesive and progressive introduction to intermediate-level Haskell, which is something that a collection of blog posts and articles by different authors written in different GHC versions cannot achieve. Although I am still far from being proficient in Haskell, the book has given me a solid foundation to confidently delve into more advanced Haskell concepts.

Now, where should I go from here? The author offers some recommendations for further study, such as [Parallel and Concurrent Programming in Haskell](https://simonmar.github.io/pages/pcph.html) for exploring concurrency, [Functional Design and Architecture](https://www.manning.com/books/functional-design-and-architecture) for industry-level design, and [Type-Driven Development with Idris](https://www.manning.com/books/type-driven-development-with-idris) for an introduction to dependent types. I also have a few other books on my list: [Thinking with Types](https://leanpub.com/thinking-with-types) for more advanced type-level programming, [Algebra-Driven Design](https://leanpub.com/algebra-driven-design) for in-depth functional programming, and [Category Theory For Programmers](https://github.com/hmemcpy/milewski-ctfp-pdf#category-theory-for-programmers) for an introduction to the theory behind the terms used in Haskell. Hopefully, I will enjoy reading them as much as I enjoyed this book.
