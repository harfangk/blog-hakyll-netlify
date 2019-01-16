---
title: My Brief Foray into the Land of Haskell (1)
---

## The Scary, Scary Land of Haskell

I think few other programming languages evoked as much trepidation in me as did Haskell. After all, it is a language known for being arcane and unusual. Still, everyone said that learning Haskell would dramatically broaden my understanding of programming, so I've been eyeing for an opportunity to venture into the land of Haskell for a while.

<!--more-->

## Setting Up the Plan

It seems that Haskell is hard to learn because of two reasons:

* Haskell is wildly different from mainstream imperative languages.
* Resources for learning Haskell are terrible in terms of beginner-friendliness.

Well, I can't do anything about the first reason because that arises from the nature of Haskell. But the second reason sounded familiar. I've seen the same pattern back in college. A professor knowledgeable in her area can be terrible at teaching due to lack of training in pedagogy. Those who teach Haskell also tend to be quite intelligent, so I suspect they fall into the same trap. And I've already suffered from such professors in college, so I wasn't keen on subjecting myself to the same kind of torture again. I had to find a good guidance.

Then I came upon an article titled [Functional Education](http://bitemyapp.com/posts/2014-12-31-functional-education.html) by @bitemyapp. The author discussed the problems in learning functional programming, and reviewed many existing resources for learning Haskell. I did not, and still do not, know enough to evaluate his review. But he sounded rational enough that I decided to follow his recommendation.

His recommendation was to begin with Brent Yorgey's [cis194 course](http://www.seas.upenn.edu/~cis194/spring13/lectures.html) first and then take [Data61 course](https://github.com/data61/fp-course).

Now I finally knew where to start. Time to venture forth.

## Flashback: My Existing Toolbelt

As I was learning Haskell, I realized that I was actually quite well equipped to learn it. Although I had barely a year under my belt as a professional software developer, my path somehow exposed me to many foundational concepts of Haskell. Specifically, I was already familiar with or exposed to the following concepts: higher order function, recursion, type inference, parametric polymorphism, lazy evaluation, fold, and pattern matching.

I started my first serious software development with Ruby. Although Ruby is an objected-oriented language, it makes heavy use of higher order functions through blocks. Functions like `map` or `reduce`, which are basic patterns of functional programming, are also commonly used in Ruby.

Then I briefly worked with Swift for iOS development. Swift promotes using immutable data unless required otherwise. It also makes heavy use of higher order functions as callbacks. Swift also provides compile time type checking and type inference. Although I haven't really used generic programming in Swift, it did give me a chance to expose myself to it.

My next serious involvement was with Elixir. Pattern matching and recursion are bread and butter of Elixir so I was comfortable with them. I got more familiar with immutable data structures because Elixir, unlike Swift, provided no mutability whatsoever. I also gained deeper understanding of folding, which I've used in Ruby without really understanding it. I've known about the concept of lazy evaluation, but I first started actually using it in Elixir.

Still, I didn't know that I already had all these ingredients ready when I dived into Haskell. Somehow the dots got connected in the end, I guess.

## 5/5 Would Recommend CIS194 (SP 13)

I liked the conciseness of the course material. It hit the right balance between terseness and verbosity. The instructor also provided relevant chapters from [Learn You a Haskell for Great Good](http://learnyouahaskell.com/) and [Real World Haskell](http://book.realworldhaskell.org/) as suggested reading each week. When the explanation in the course material felt too short, reading those chapters gave me an opportunity learn more slowly and thoroughly. It's my favorite combination of learning materials - concise core material complemented by extensive optional resources.

But I think the best thing about this course is homeworks. They are definitely challenging. I confess that I looked at the solutions available online when I couldn't even think of how to get started on some homeworks. Nevertheless, all homeworks can be solved just with the concepts introduced in the course material. This is actually an impressive feat - Yorgey made challenging homeworks out of the concise course material. You would understand how hard that is if you've tried to create good exercise problems yourself.

So I would recommend this course to someone who wants to learn Haskell. But there's a caveat. If you are completely new to functional programming, get a mentor or teacher. This course is still too fast-paced for such a person. I have a personal rule of thumb when choosing a learning material for learning by myself: I should be able to understand 70% of the material with ease, and actively learn only the other 30%. Otherwise, I would end up losing motivation out of exhaustion. I believe that I could finish this course on my own because I was already familiar with the basic concepts I've mentioned above. Otherwise, I would have quit the course midway.

If you are trying to learn functional programming by youself, I recommend starting with Elixir. Elixir is a simple, practical language that can familiarize you with basic concepts of functional programming. Once you have knowledge of those basic building blocks, learning more advanced functional programming concepts in Haskell would feel much less stressful.

In the next post, I will describe what I learned from my adventure in the land of Haskell.
