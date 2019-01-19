---
title: Elixir as First Programming Language (Part 1)
---

*This post is adapted from my talk at Seoul Elixir Meetup on November 16, 2016.*

Elixir might not be the first language that comes to your mind when you think about which programming language to teach to beginners. Languages like Python, Ruby, JavaScript, C/C++, or Java are more popular choices. But I think Elixir has some unique advantages that makes it a serious candidate.

<!--more-->

## Learning to program is very, very hard

For experienced developers, it's hard to truly understand  how hard it is for beginners to learn to program. You've  forgotten all the obstacles that you've overcome to gain the current level of proficiency. But if you try teaching programming to beginners, you can see that so many things that seem obvious to you are incomprehensible to them.

Programming requires inordinate amount of underlying knowledge and training. Some beginners have difficulty typing, even in hunt and peck style, and some others have rarely worked with long texts and find them unbearable. It is in this hostile, alien terrain that beginners have to learn new difficult concepts.

That's why learning to program is so brutally difficult. We should always keep this in mind.

## Procedural and object-oriented languages are not ideal for beginners

I am not arguing that one paradigm is technologically or conceptually better than another. Here I'm strictly talking in terms of teaching and learning.

When teaching something difficult, the key is to break down difficult parts into small bits and spread them across a period of time so that students wouldn't be overwhelmed. This is even more critical for beginners, who are usually intimidated by the new concepts and lack confidence. 

Procedural programming and object-oriented programming, the two commonly taught paradigms, make such adjustment difficult. They give teachers little freedom to ease the learning curve that students have to climb.

In general, I've found that students have little problem understanding fundamental concepts like variable, constant, or function. They can understand what `=` means. Of course, I'm not teaching them whether that's an assignment or a binding, because such details do not matter at that stage.

### Teaching procedural programming

The problem shows up in the next stage. Procedural languages are low-level oriented, so they introduce the concept of pointers and memory locations. In order to explain those concepts, hardware should be brought into the curriculum. 

Hardware and software are related but different subjects. Students are forced to learn both of them and how they interact with each other at the same time. This results in an extremely steep learning curve. It's no wonder that so many students quit when they encounter pointers.

Breaking down the concept into smaller bits usually remedies this kind of problem, but that's not an option when explaining what a pointer is. How can you explain what a pointer is without explaining how memory works? In this area the topics are so tightly coupled that it's impossible to break them apart.

I think procedural programming should be taught in a different way. Students should learn about either hardware or software first, and then learn procedural programming to complement their existing knowledge. 

Learning hardware first and then learning procedural programming would be the classic electrical computer engineering curriculum. Learning software first would be better done with functional programming or object-oriented programming, because procedural programming invariably involves hardware.

Because we're talking about learning to program, I'll explore the second choice. That leaves object-oriented programming and functional programming.

### Teaching object-oriented programming

Object-oriented programming poses a different challenge: objects. Object is a concept that has too high level of abstraction that is introduced too early in the curriculum. 

What is an object? According to a [wikipedia article](https://www.wikiwand.com/en/Object_(computer_science)), "an object can be a variable, a data structure, or a function or a method, and as such, is a location in memory having a value and possibly referenced by an identifier."

How many underlying concepts do students need to understand before they can understand what an object is? Let's try counting. Variable, data structure, function, location in memory, value, reference, and identifier. That's seven concepts, plus the mental effort of building an abstraction on top of them. 

It's unreasonable to expect people who started programming just this month to understand something that complicated. Yet object is the basic building block of object-oriented programming, so it must be taught before students get to start writing codes. Skipping it or pushing it back in the curriculum is not an option.

The only option left is to simplify it, leaving out a large part of the original abstraction. I think this is where simpler definitions like "objects are things" or "objects are nouns" or "objects are instances of classes" come from.

Compare them with how Alan Kay originally envisioned the objects: "I thought of objects being like biological cells and/or individual computers on a network, only able to communicate with messages." The difference in the levels of abstraction is almost dizzying.

To adapt a common software term, such short-term approach incurs "educational debt." When students try to use their incomplete understanding of object in languages that are designed using the complete abstraction of object, things do not work together seamlessly. This has been a great source of distress for many new programmers, including me.

I still don't know how I would teach object-oriented programming to beginners properly. The concept of object is just too difficult for beginners to understand without extensive background in software development and history of computer science. 

### Teaching Elixir

As a functional programming langauge, Elixir doesn't directly involve hardware. Moreover, its basic building block is function, which is much easier to teach and learn than object. 

Among functional programming languages, Elixir is a simpler one. It does not include those esoteric concepts in functional programming like monad, type class, or currying. Most of the time, it just uses plain functions, and stays with it.

That's potentially the most valuable property of Elixir as a first language. When you write code, you're mostly composing and piping simple functions, not building recursively abstracter things like type classes or monads.

This property of staying at single level of abstraction has two benefits. Students can concentrate on just one thing and step by step learn new ways to compose functions. Teachers retain a lot of freedom when planning a curriculum that has concepts of mostly equivalent difficulty level.

Plus, it's still possible to teach advanced concepts like monad or macro later in the curriculum.

Elixir also provides a few niceties that make learning easier. For example, its pipe operator allows writing  function composition from left to right, not the other way. That's much more intuitive for people who use human languages that are written left to right. I also like its pipe operator, which completely mirrors the eye movement when reading code.

```elixir
foo(bar(baz(new_function(other_function()))))

other_function() |> new_function() |> baz() |> bar() |> foo()

other_function() 
|> new_function() 
|> baz() 
|> bar() 
|> foo()
```

Its dynamic type system also helps in this respect, because it allows students to start writing codes without having to know all the types in Elixir and their perks. Static type system is not that bad if it has good inference system, but dynamic type system is still easier.

In addition, Elixir is a language that's focused on productivity. Here productivity means you need to type fewer characters to get feedback or accomplish something. This means that students can have quick feedback loop. Also they get to quickly write applications that they can show their friends or families. This is really helpful because it boosts their motivation greatly.

Elixir has its own quirky areas. `char list`, `binary`, and `string` are going to confuse people. `[69, 108, 105, 120, 105, 114]` returning `'Elixir'` makes little sense. Understanding its OTP behaviors are at least as challenging as understanding what an object is. Moreover, it has entire Erlang/OTP hidden under the surface. Still, I'm convinced that Elixir as a teaching language has a lot of promise.

## Summary of Part 1

1. Learning to program is really difficult for beginners
2. It's important for teachers to be able to adjust the pace and difficulty of learning
3. Procedural and object-oriented programmings have crucial elements that make such adjustment difficult
4. Elixir is much simpler than other functional programming languages and has high productivity
