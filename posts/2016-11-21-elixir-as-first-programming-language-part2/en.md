---
title: Elixir as First Programming Language (Part 2)
---

*This post is adapted from my talk at Seoul Elixir Meetup on November 16, 2016.*

Elixir might not be the first language that comes to your mind when you think about which programming language to teach to beginners. Languages like Python, Ruby, JavaScript, C/C++, or Java are more popular choices. But I think Elixir has some unique advantages that makes it a serious candidate.

<!--more-->

## Elixir teaches new developers the practice of breaking codes into smaller parts

### My story with Ruby on Rails

Similar to what many people experienced with Rails, my Rails app eventually started to resemble a big ball of mud. Ever since I left my previous job, my quest was to find out how and why it ended up like that.

After some research, I came up with an acceptable answer. Rails provides a set of great conventions, commonly called "the Rails Way", which focuses on empowering developers to quickly create basic CRUD web applications. They provide very sensible default structure for web applications, taking care of lots of details including database, routing, and asset management. 

Unfortunately, as the app grows more and more complex, "the Rails Way" becomes less effective. Its focus on short-term productivity inadvertantely promoted code organization structure that's great for simple short-term projects, but less appropriate for complex long-term projects. 

This has caused a lot of pain to thousands of developers, including me, who stayed with "the Rails Way" far too long when it was no longer the best choice. For a self-taught beginner like me, "the Rails Way" was the only code structure I knew so I didn't even have any alternative.

I kept on researching what could be an alternative way to organize code. It seems that after a lot of experimentations, people did come up with unofficial best practices for growing a Rails app.

* Controllers should only dispatch HTTP calls
* Views and helpers should only handle rendering view
* Move everything else into model layer
* Separate persistence and domain logics in model layer
* Do proper OOP with plain Ruby objects

In other words, break apart code and keep things simple. After reading a few books, I also found out that this is actually a familiar mantra that has been repeated at least since 1990s. Unfortunately, there was no easy way for me to learn about that. I started to wish that there was a good way to learn this on my own. That's when I learned about Elixir, and after working with Elixir for a bit, I saw its great potential at teaching such way of code organization.

### Elixir "teaches" you to break apart codes

Elixir provides language level support to break code into smaller parts and practically prods you into doing so. 

At module level, Elixir has pattern matching. It discourages creating colossal case statements or if-else statements. At program level, Elixir has OTP. It discourages building bloated monolithic applications.

Other languages also support separating and managing code, but Elixir takes it to the next level. Elixir seems to consider code separation as one of its main priorities, and makes it really easy and desirable to do so.

I believe that language and framework matter a lot, because they determine how programmers think and write code. What a language or framework makes easy and accessible affects us much more than we tend to believe. Human brain loves to use heuristic, preferring easy mental shortcuts over more rigorous analysis. When a language or a framework makes something easier than another, our brain definitely takes that easy path. 

Software developers are no exception. What I found amazing during my research about code organization in Rails is that so many experienced developers kept following the "opinions" of Rails despite the pain it's causing them due to the mismatch between Rails' and their own goals. They kept following it until literally they couldn't take it anymore. Why? Because it was far easier to follow the conventions than to devise a new way.

That may sound like a bad news, but it also means that right design can be unbelievably helpful. Despite all my criticism about code organization aspect of Rails conventions, they are great examples with regards to setting up web applications. They greatly improved the entire landscape of web development.

All this means that Elixir developers will split code with very high probability whenever they can, just because Elixir makes it easy to split code. Such design will benefit both experienced and novice developers when writing code. But in terms of learning, it won't help experienced developers much because they already have that skill. But for new developers, this means that they get to learn and practice such a valuable skill. I wish I could have done that.

Moreover, Elixir community and literature also emphasize concurrent distributed programming, promoting separation of code. So new developers have their text editors, co-workers, Elixir documentations, and random guy on the Internet telling them to break apart their code. That's a great environment to quickly learn and internalize something.

I think this provides some benefits to senior developers and companies too. It means that new developers are less likely to build big balls of mud that senior developers have to fix. It also means that it would cost less time and money to train new developers to use best practices.

## Disadvantages

Despite all my fawning, teaching Elixir as a first language has some significant drawbacks.

The most critical problem is that there are few jobs for Elixir yet. When new people decide to learn programming, they don't do it for fun. They are making serious investment of their time and effort, so they want commensurate return over investment. Unfortunately, Elixir cannot promise this in short term. I see great potential in long term, though.

Elixir also has few learning material. There are only a few tutorials, several books, a few online courses, and a few screencasts so far. They are all in English too so non-English speakers would have difficulty learning it. 

Elixir also has no IDE. When beginners learn to program, having IDEs with that familiar GUI interface reassures them a lot. There are a few Elixir REPL websites, but in the end you have no other option but to use austere text editors.

## Summary of Part 2

1. Rails is a great example how much the "opinions" of a language or a framework can affect developers, especially new ones.
2. Elixir "encourages" you to break apart codes through its built-in conventions and tools.
3. Elixir teaches new developers the practice of breaking codes into smaller parts, which could also benefit senior developers and companies.
4. Elixir has few jobs or learning material in short term, but I expect it to grow quickly.
