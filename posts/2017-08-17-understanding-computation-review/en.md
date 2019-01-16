---
title: Understanding Computation - Review
---

## Summary

If you are a software developer without formal computer science background who want a friendly introduction to computer science, I highly recommend this book.

This book explores some of core concepts of computer science, specifically syntax and semantics of programming languages and basic abstract machines that can execute programs. The author writes in a concise, easy-to-read, and lighthearted style, providing codes that you would write at your daily job as examples. It's much easier to read than a typical academic writing that's full of bizarre mathematical symbols.

<!--more-->

## What Makes It Special

What I found most impressive was its easy-to-read style. I could just read through the book because the author intentionally avoided using convoluted expressions or obscure jargons unless it's absolutely necessary. In contrast, when I read other theoretical books, I usually have to go back and forth between pages to keep up with the content. He also stays quite down-to-earth and oddly humorous, living up to the British stereotype. For example, after introducing mathematical description of semantics of a simple language takes up a whole page, he follows up with these words:

> Mathematically speaking, this is a set of inference rules that defines a reduction relation on SIMPLE’s abstract syntax trees. Practically speaking, it’s a bunch of weird symbols that don’t say anything intelligible about the meaning of computer programs.

The book also provides concrete examples written in actual Ruby code for each abstract concept it introduces. I think this is what sets this book apart from other books on computer science. Source code is the language that software developers, the intended target audience of this book, are most comfortable with. Those examples demonstrate abstract concepts in a way that software developers can most easily understand.

For example, this is one of the code implementations the author provides when he explains the concept of operational semantics and how a program gets evaluated and reduced to its final value:

```ruby
>> Machine.new(
 LessThan.new(Number.new(5), Add.new(Number.new(2), Number.new(2)))
  ).run
  5 < 2 + 2
  5 < 4
  false
  => nil
```

The author doesn't stop at simple examples. In the last part of the book, he explains the halting problem and demonstrates it with a Ruby program, not with abstract mathematical thought exercise. I thought this was both incredibly brilliant and easy to understand. Here's the demonstration:

```ruby
# do_the_opposite.rb

def halts?(program, input)
  # parse program
  # analyze program
  # return true if program halts on input, false if not
end

def halts_on_itself?(program)
  halts?(program, program)
end

program = $stdin.read

if halts_on_itself?(program)
  while true
  # do nothing
  end
end
```

> This code reads program from standard input, finds out whether it would halt if run on itself, and promptly does the exact opposite: if program would halt, do_the_opposite.rb loops forever; if program would loop forever, do_the_opposite.rb halts. Now, what does ruby do_the_opposite.rb < do_the_opposite.rb do? Just as we saw earlier with does_it_say_no.rb, this question creates an inescapable contradiction.

Explaining a new concept in a easy to understand manner is already an impressive feat. Writing a whole book that introduces numerous complex concepts while still keeping it easy to read is definitely worthy of applause. So I cannot but appreciate the author's meticulous effort at catering to the intended audience of his book.

## What Topics Does It Cover

The book covers a range of topics, mostly those from programming language theory and models of computation. Interestingly, the book has a kind of narrative structure instead of simply listing technical concepts.

At first it briefly introduces various ways for understanding the meaning of a program, such as operational semantics or denotational semantics. Then it describes simple abstract machines, such as finite automata or pushdown automata, that can execute those programs, while explaining how to parse the programs into a format that machines can execute.

Afterwards the book introduces increasingly powerful abstract machines, culminating in a universal Turing machine. Then it explores properties of universal Turing machine by comparing it with other universal systems like lambda calculus, cyclic tag system, or iota language. Finally it analyzes the limitations of universal Turing machines and introduce undecidable problems that cannot be generally solved by universal Turing machine, which is the most powerful computer we can currently build.

That's a lot of complex topics to cover in a book that's slightly longer than 300 pages. Still, the author lays it all out in such a concise manner that you can understand the topics to the extent that the next section requires you to.

## What I Learned

### The Meaning of Program

As a newcomer into software development, I noticed that the more I study, the more there is to learn in this field. Horizontally, there's an ever-increasing number of different ways of doing things. There are new cool languages, yet another JavaScript framework, new ways of deployng to AWS, better ways of writing API, and so on. Then again, vertically, there are abstraction layers that reach deeper than nine circles of Hell. I write in high level language, which gets parsed, then compiled to bytecode, which is converted to machine code, that gets executed at CPU, which is built with gates, and so on. Why, just today I learned that below TCP/IP lie 7 layers of Open Systems Interconnection. I guess even nine circles are not deep enough for this field.

Because I was overwhelmed by the number of things to learn, I couldn't even think about what all these things aim to achieve in the end. The book taught me that they could be abstracted away as implementation details, and that there exists a bigger context, that is, the meaning and intention of a program. Without thinking about that ultimate goal, there was no way I could fully understand the goals and structures of these individual pieces of technology.

In terms of human communication, it was like being too immersed in technical details of communication: how is a sentence structured grammatically; how is a spoken sentence vocalized; or how does the human auditory system work. They are valuable topics of their own, but without taking account of their ultimate goal of conveying ideas among human beings, our understanding of the topics would be incomplete at best.

This new way of thinking about program was the most valuable lesson I learned from reading this book.

### Impossibility of Writing a Bug-Free Program

Discussion about undecidable problem also gave me answers to some lingering questions I had while writing code. For example, as I wrote tests for my Ruby program, I wondered about how many tests I would have to write until I could trust that my program would run without error. Is it possible to reach a point like that if I kept writing more test cases? Now I know that the answer is a negative, according to Rice's theorem that any non-trivial property of program behavior is undecidable.

When I first learned Haskell, I was thrilled by the slogan that "if it compiles, it works". They said that the compiler guarantees that the program would contain no errors. But then it was still possible to have runtime errors in Haskell. How come? I shrugged the question away, just thinking that no system is perfect. Now I have theoretical understanding that such a guarantee is fundamentally impossible. So even though a rigorous type system can drastically reduce a category of errors, it still cannot eliminate all errors.

So all the techniques and best practices for making bug-free software were futile by nature. While I do find this Sisyphean endeavor tragically beautiful in some way, I find it a bit disappointing and depressing. I hoped for something more definite in software development than in my previous field of international security. I can accept it as a fact of life, but I feel as if that cake, which I was promised with, has turned out to be a lie.

### First Among Equal Programming Languages

I had read that most programming languages are Turing complete, which meant that any Turing complete language could be used to do anything another one could do, but I didn't give much thought to it. That statement started to make more sense after I've read this book. After seeing that vastly different systems like a Turing machine, lambda calculus, rule 110 system, or other still more weird systems have equivalent power, I found it easier to accept that most of widely used languages are equivalent in power. This meant that all that bickering about which language is more powerful is nothing more than waste of time.

But if they are all equivalent, what other technical criteria should we use to choose which language to use? For some projects, there exists just one reasonable choice. For example, C/C++ is the only reasonable language for console game development. JavaScript is the only language that runs on a web browser. For most other situations, however, there are a lot of languages to choose from. As far as I know, the choice mostly depends on personal preference of the CTO, or operational requirements like the ease of hiring developers in the local area. But I wonder if there exist some fundamental technical criteria that can provide the answer from engineering perspective.

## Grand Trolling

Finally, I want to share a beautiful piece of trolling the author throws at the readers. In the middle of the book, the author implements FizzBuzz using lambda calculus implemented only with Ruby's anonymous funtions. At the end of the chapter, the author gives a beautiful FizzBuzz program that spans over four pages.

I want to note that, however, this is not some meaningless trolling. The author thoroughly demonstrates how to implement each part of this code, so if you actually read the book, you can actually discern some of the moving parts in the code. So this is rather a cheeky pat in the back by the author to the readers who have followed him so far.

You can copy the following code to a Ruby file and run it to see if it is actually a working FizzBuzz program.

Now enjoy the code:

```ruby
=begin
FizzBuzz program with linebreaks

-> k { -> f { -> f { -> x { f[-> y { x[x][y] }] }[->
x { f[-> y { x[x][y] }] }] }[-> f { -> l { -> x { -> g { -> b { b }[-> p { p[->
x { -> y { x } } ] }[l]][x][-> y { g[f[-> l { -> p { p[-> x { ->
y { y } } ] }[-> p { p[-> x { -> y { y } } ] }[l]] }[l]][x][g]][-> l { ->
p { p[-> x { -> y { x } } ] }[-> p { p[-> x { -> y { y } } ] }[l]] }[l]][y] }]
} } } }][k][-> x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][-> x { ->
y { x } }]][-> l { -> x { -> l { -> x { -> x { -> y { -> f { f[x][y] } } }[->
x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[l][f[x]] } }]
} }[-> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[->
f { -> m { -> n { -> b { b }[-> m { -> n { -> n { n[-> x { -> x { ->
y { y } } }][-> x { -> y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[->
g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]]
} }[m][n]][-> x { -> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { ->
y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[f[-> n { -> p { ->
x { p[n[p][x]] } } }[m]][n]][m][x] }][-> x { -> y { -> f { f[x][y] } } }[->
x { -> y { x } }][-> x { -> y { x } }]] } } }][-> p { -> x { p[x] } }][->
p { -> x { p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p
[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p
[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[x]]]]]]]]]]]]]]]]]]]]]]
]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]
} }]][-> n { -> b { b }[-> n { n[-> x { -> x { -> y { y } } }][-> x { ->
y { x } }] }[-> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }]
}[-> f { -> m { -> n { -> b { b }[-> m { -> n { -> n { n[-> x { -> x { ->
y { y } } }][-> x { -> y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[->
g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]]
} }[n][m]][-> x { f[-> m { -> n { n[-> n { -> f { -> x { n[-> g { ->
h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]][n][x] } ][m]
} } }][n][-> p { -> x { p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[x]]]]]]]]]]]]]]]
} }]]][-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][->
x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { ->
f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y]
} } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { ->
y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { ->
x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { ->
f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y]
} } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[->
l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { ->
y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y]
} } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[->
l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { ->
y { -> f { f[x][y] } } }[x][l]] } }[-> x { -> y { -> f { f[x][y] } } }[->
x { -> y { x } }][-> x { -> y { x } }]][-> n { -> p { -> x { p[n[p][x]]
} } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]]
} } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[->
n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[->
p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { ->
p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { ->
p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { ->
n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][->
p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]]
} }]]]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { ->
x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[->
m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { ->
x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]]
} }]]]]]][-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]]
} } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { ->
x { p[p[p[p[p[x]]]]] } }]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { ->
p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { ->
p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { ->
x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]]
} }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]]
} } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]]
} } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[->
n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[->
p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { ->
p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { ->
n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][->
p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]]
} }]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { ->
n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }]
} }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]][->
b { b }[-> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> f { ->
x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[-> f { -> m { ->
n { -> b { b }[-> m { -> n { -> n { n[-> x { -> x { -> y { y } } }][-> x { ->
y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]]
} }][-> y { x }][-> y { y }] } } }][m] } }[m][n]] } }[n][m]][-> x { f[-> m { ->
n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }]
} } }][m] } }[m][n]][n][x] }][m] } } }][n][-> p { -> x { p[p[p[x]]] } }]]][->
l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { ->
y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y]
} } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[->
l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { ->
y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y]
} } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[->
x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][-> x { -> y { x } }]][->
n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[->
n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[->
m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m]
} }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { ->
x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { ->
p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { ->
p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { ->
x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]]
} }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]]
} } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[->
n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[->
p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]][-> n { ->
p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { ->
x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]]
} }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]][-> b { b }[-> n { n[-> x { ->
x { -> y { y } } }][-> x { -> y { x } }] }[-> f { -> x { f[-> y { x[x][y] }]
}[-> x { f[-> y { x[x][y] }] }] }[-> f { -> m { -> n { -> b { b }[-> m { ->
n { -> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> m { ->
n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }]
} } }][m] } }[m][n]] } }[n][m]][-> x { f[-> m { -> n { n[-> n { -> f { ->
x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m]
} }[m][n]][n][x] }][m] } } }][n][-> p { -> x { p[p[p[p[p[x]]]]] } }]]][->
l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { ->
y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y]
} } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[->
l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { ->
y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y]
} } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[->
x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][-> x { -> y { x } }]][->
n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[->
n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[->
m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m]
} }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { ->
x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { ->
p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { ->
p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { ->
x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]]
} }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]]
} } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]]
} } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m]
} }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { ->
x { p[p[p[p[p[x]]]]] } }]]]]]][-> m { -> n { n[-> m { -> n { n[-> n { -> p { ->
x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]]
} }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]][-> f { -> x { f[-> y { x[x][y] }]
}[-> x { f[-> y { x[x][y] }] }] }[-> f { -> n { -> l { -> x { -> f { ->
x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[-> f { -> l { ->
x { -> g { -> b { b }[-> p { p[-> x { -> y { x } }] }[l]][x][-> y { g[f[->
l { -> p { p[-> x { -> y { y } }] }[-> p { p[-> x { -> y { y } }] }[l]]
}[l]][x][g]][-> l { -> p { p[-> x { -> y { x } }] }[-> p { p[-> x { ->
y { y } }] }[l]] }[l]][y] }] } } } }][l][-> l { -> x { -> x { -> y { ->
f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y]
} } }[x][l]] } }[-> x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][->
x { -> y { x } }]][x]][-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[->
x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }] } }[->
b { b }[-> m { -> n { -> n { n[-> x { -> x { -> y { y } } }][-> x { ->
y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]]
} }][-> y { x }][-> y { y }] } } }][m] } }[m][n]] } }[n][-> n { -> f { ->
x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }[-> m { ->
n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][->
p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]]
} }]]]][-> x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][-> x { ->
y { x } }]][-> x { f[-> f { -> x { f[-> y { x[x][y] }] }[-> x { f[->
y { x[x][y] }] }] }[-> f { -> m { -> n { -> b { b }[-> m { -> n { -> n { n[->
x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> m { -> n { n[-> n { ->
f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m]
} }[m][n]] } }[n][m]][-> x { -> n { -> p { -> x { p[n[p][x]] } } }[f[-> m { ->
n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }]
} } }][m] } }[m][n]][n]][x] }][-> p { -> x { x } }] } } }][n][-> m { ->
n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][->
p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]]
} }]]][x] }]][-> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }]
}[-> f { -> m { -> n { -> b { b }[-> m { -> n { -> n { n[-> x { -> x { ->
y { y } } }][-> x { -> y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[->
g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]]
} }[n][m]][-> x { f[-> m { -> n { n[-> n { -> f { -> x { n[-> g { ->
h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]][n][x] }][m]
} } }][n][-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]]
} } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { ->
x { p[p[p[p[p[x]]]]] } }]]] } }][n]]]] }]
=end

fizzbuzz_program =
  -> k { -> f { -> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[-> f { -> l { -> x { -> g { -> b { b }[-> p { p[-> x { -> y { x } } ] }[l]][x][-> y { g[f[-> l { -> p { p[-> x { -> y { y } } ] }[-> p { p[-> x { -> y { y } } ] }[l]] }[l]][x][g]][-> l { -> p { p[-> x { -> y { x } } ] }[-> p { p[-> x { -> y { y } } ] }[l]] }[l]][y] }] } } } }][k][-> x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][-> x { -> y { x } }]][-> l { -> x { -> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[l][f[x]] } }] } }[-> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[-> f { -> m { -> n { -> b { b }[-> m { -> n { -> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]] } }[m][n]][-> x { -> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[f[-> n { -> p { -> x { p[n[p][x]] } } }[m]][n]][m][x] }][-> x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][-> x { -> y { x } }]] } } }][-> p { -> x { p[x] } }][-> p { -> x { p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[x]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]] } }]][-> n { -> b { b }[-> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[-> f { -> m { -> n { -> b { b }[-> m { -> n { -> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]] } }[n][m]][-> x { f[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]][n][x] } ][m] } } }][n][-> p { -> x { p[p[p[p[p[p[p[p[p[p[p[p[p[p[p[x]]]]]]]]]]]]]]] } }]]][-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][-> x { -> y { x } }]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]][-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]][-> b { b }[-> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[-> f { -> m { -> n { -> b { b }[-> m { -> n { -> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]] } }[n][m]][-> x { f[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]][n][x] }][m] } } }][n][-> p { -> x { p[p[p[x]]] } }]]][-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][-> x { -> y { x } }]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]][-> b { b }[-> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[-> f { -> m { -> n { -> b { b }[-> m { -> n { -> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]] } }[n][m]][-> x { f[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]][n][x] }][m] } } }][n][-> p { -> x { p[p[p[p[p[x]]]]] } }]]][-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][-> x { -> y { x } }]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]]][-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> n { -> p { -> x { p[n[p][x]] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]]]][-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]][-> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[-> f { -> n { -> l { -> x { -> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[-> f { -> l { -> x { -> g { -> b { b }[-> p { p[-> x { -> y { x } }] }[l]][x][-> y { g[f[-> l { -> p { p[-> x { -> y { y } }] }[-> p { p[-> x { -> y { y } }] }[l]] }[l]][x][g]][-> l { -> p { p[-> x { -> y { x } }] }[-> p { p[-> x { -> y { y } }] }[l]] }[l]][y] }] } } } }][l][-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }[-> x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][-> x { -> y { x } }]][x]][-> l { -> x { -> x { -> y { -> f { f[x][y] } } }[-> x { -> y { y } }][-> x { -> y { -> f { f[x][y] } } }[x][l]] } }] } }[-> b { b }[-> m { -> n { -> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]] } }[n][-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }[-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]]][-> x { -> y { -> f { f[x][y] } } }[-> x { -> y { x } }][-> x { -> y { x } }]][-> x { f[-> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[-> f { -> m { -> n { -> b { b }[-> m { -> n { -> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]] } }[n][m]][-> x { -> n { -> p { -> x { p[n[p][x]] } } }[f[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]][n]][x] }][-> p { -> x { x } }] } } }][n][-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]][x] }]][-> f { -> x { f[-> y { x[x][y] }] }[-> x { f[-> y { x[x][y] }] }] }[-> f { -> m { -> n { -> b { b }[-> m { -> n { -> n { n[-> x { -> x { -> y { y } } }][-> x { -> y { x } }] }[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]] } }[n][m]][-> x { f[-> m { -> n { n[-> n { -> f { -> x { n[-> g { -> h { h[g[f]] } }][-> y { x }][-> y { y }] } } }][m] } }[m][n]][n][x] }][m] } } }][n][-> m { -> n { n[-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }[m]][-> p { -> x { x } }] } }[-> p { -> x { p[p[x]] } }][-> p { -> x { p[p[p[p[p[x]]]]] } }]]] } }][n]]]] }]

# Helpers

LEFT  = -> p { p[-> x { -> y { x } } ] }
RIGHT = -> p { p[-> x { -> y { y } } ] }
IF    = -> b { b }
IS_EMPTY  = LEFT
FIRST     = -> l { LEFT[RIGHT[l]] }
REST      = -> l { RIGHT[RIGHT[l]] }

def to_integer(proc)
  proc[-> n { n + 1 }][0]
end

def to_boolean(proc)
  IF[proc][true][false]
end

def to_array(proc)
  array = []

  until to_boolean(IS_EMPTY[proc])
    array.push(FIRST[proc])
    proc = REST[proc]
  end

  array
end

def to_char(c)
  '0123456789BFiuz'.slice(to_integer(c))
end

def to_string(s)
  to_array(s).map { |c| to_char(c) }.join
end

# Print result

to_array(fizzbuzz_program).each do |p|
  puts to_string(p)
end
```
