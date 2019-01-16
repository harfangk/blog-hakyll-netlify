---
title: My Brief Foray into the Land of Haskell (2)
---

Nowadays many features of Haskell can be found in other languages, too. But such features often feel like an awkward addition to languages that are designed for different goals in mind. In contrast, functional programming elements are given the highest accommodation in Haskell. I think this is what makes Haskell special - it is purely dedicated to functional programming. So even though Haskell and other languages might be providing theoretically identical features, the experience of using them is so vastly different. I think I could learn the following lessons because of this special atmosphere that Haskell has.

<!--more-->

## Object Classes Are Not Types

I began learning software development with Ruby. In Ruby, everything is an object. An integer value is an instance of the class `Integer`. Likewise, a string value is an instance of the class `String`. I took this approach to heart. An unfortunate side-effect was, however, that I ended up considering the class of an object and the type of an object as synonyms. After all, they made little difference in Ruby in practice. 

Haskell completely dispelled my misunderstanding. I now understand that an object is a special kind of data type. Ruby was special in that it was a Smalltalk-style language that operated on object level abstraction instead of data type level abstraction. 

My brief experience with Java left me thinking that primitive data types were not as powerful as proper object types. Haskell showed me that powerful abstractions can be built on 'primitive' data types, such as algebraic data types or type classes. This opened up a whole new kind of perspective for me. 

## The Perfectionist Compiler

My first encounter with a compiler was in Java. I hated it. To me, the compiler was an evil thing that forced me to add unnecessary ceremonies to my code and pedantically criticized me for simple mistakes. Swift compiler was much better. It had type inference so it didn't force me to annotate the type for every single variable. It made me adopt a debugging style different from the `puts` oriented style that I was used to in Ruby, but it was okay. 

Haskell compiler was a whole new beast. It required a much higher level of correctness at compile time than other any other compiler I've worked with. I'm not saying that a Haskell program is more correct than programs written in other languges. After all, all programs should be correct. Otherwise they won't work. But interpreted languages like Ruby or Javascript check the correctness of code almost entirely at runtime. Even compiled languages like Java and Swift defer a lot of correctness checking to runtime. Haskell was different in that it required rigorous correctness immediately upfront at compile time. 

Such a characteristic of Haskell compiler made me approach programming differently. I started to think over the program more rigorously, as if trying to sastisfy the standards set by Haskell compiler before I wrote the code. Of course I could never come close to the demanded level of correctness. But I was certain that if I kept coding in Haskell, I could improve my abstract modeling skill to much higher level than I could with in any other language.

## Generic Programming At Its Finest

I first encountered generic programming in Java, but I didn't understand it at the time. My first impression was that it felt incredibly clunky and convoluted. Then I came across generic programming once more in Swift later. I've learned much more about programming in the meantime and saw more use in it. But it felt rather out of place in Swift and still required too many ceremonies to use. 

Haskell showed me the full power of generic programming, more commonly called parametric polymorphism in Haskell. I think there is little difference in terms of capabilities of generic programming between these languages. But Haskell was built around parametric polymorphism, whereas Java and Swift adopted it into its existing model. This caused a critical difference in using generic programming in Haskell and other languages. Thanks to Haskell, I could experience generic programming in its full glory.

## Lazy Thinking

Haskell uses lazy evaluation. I was already familiar with the concept of lazy evaulation as it was supported on Elixir. But once again, being forced to use lazy evaluation all the time is a completely different experience from optionally using lazy evaluation. When working with languages with strict evaluation, my mental model of the program was built around small and completed steps. 

But Haskell program was lazy so I could not use the same model. I could still break down my program into small steps, but I could not treat them as completed operations. I had to always hold the entire program in my mind, because lazy evaluation meant that either my entire program ran or nothing ran at all. In conjunction with the rigorous compiler, this made me keep a broader perspective in my mind all the time. 

## Monad, Monad, Monad

Monad is the most commonly cited example of Haskell's esoteric abstractions, and it did inspire certain mysterious dread in me. But after learning about it, I realized that you don't have to be so afraid of it. If you've been programming, it is highly likely that you are already familiar with the abstraction called monad. It's just that you didn't know that the abstraction is called monad in functional programming.

In a way, what's happening with monad is similar to what happened with design patterns in objected-oriented programming. Experienced developers have been using design patterns well before the Gang of Four book popularized the patterns. It's just that they haven't been using the same names for those patterns. Likewise, you are likely to already know an abstraction called monad. All you need to learn is to correctly label that abstraction in your mind so that you could recognize it.

## Conclusion

These are the takeaways from my brief excursion into Haskell. I've yet to program anything significant in Haskell, so my first impression of Haskell is likely to be wrong and limited in many places. Still, it's amazing how many new perspectives it opened for me during such a brief exposure. I look forward to writing more in Haskell.
