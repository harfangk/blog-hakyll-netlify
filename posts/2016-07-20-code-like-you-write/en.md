---
title: Code like you write
---

"Write" is the verb we use with programming. We *write* software. We *write*
codes. Of course, we use other verbs too. We also build and design software.
But write is the one we use most frequently. Is there some kind of
philosophical meaning behind this usage? I doubt it. But as someone who has
passion for both writing and coding, I see some interesting similarities
between these two activities that are usually considered to be diametrically
opposite. Dijkstra will probably want to strangle me for making such
a metaphor, but I think this juxtaposition could be especially helpful to those
who are coming into software development without traditional computer science
background.

<!--more-->

People tend to think of software development as something sterile, performed
with methodical precision. They imagine programmers following some kind of
meticulously drawn blueprint to the letter. The entire process is tinted
with a vague impression of silverly blue hue on metallic or glassy texture. In
reality, software development is more like a chaotic explosion of creativity,
very similar to creative writing. Their similarity is most visible in two
aspects of the process:

1. Evaluation of options and decision making
2. Constant rewriting and reiteration

### Evaluation of options and decision making

Every single step of creative writing or software development process is
a careful consideration and deliberate choice among a myriad of options that
have their own strengths and weaknesses. Which genre or platform should I use?
What are the consequences of using this metaphor or design pattern? How should
I structure these paragraphs or codes? These are just a few among such choices.
As a creator, you are responsible for every decision. And this responsibility
is as exciting and empowering as daunting and burdensome. As C.S. Lewis put it,
"You can make anything by writing." The quote is true for both coding and
writing.

### Constant rewriting and reiteration

"The first draft of anything is shit." Supposedly attributed to Earnest
Hemingway, this quote made me immediately nod in agreement. You start by
writing something that conveys what you want to express or does what you want
it to do. But then it is usually so shitty that it makes you literally cringe.
You fix and improve your writing or code until you find it to be reasonably
good enough. Or depending on the situation and personality, you improve it
until you believe you can't write any better. In writing, it's known as
revising or editing. In software development, it's known as iterative
development or refactoring. But regardless of how you put it, the process is
about constant exploration and experimentation, not a single masterful stroke. 

## A personal illustration

Let me show you how I personally do this in daily life. Please excuse my poor
examples and look at my thought processes in how I approach this.

### Deciding the big picture

First, I need a topic that I want to write about. Well, I think I shouldn't
have eaten that chocolate cookie today - or at least should have stopped at
eating just three of them. I ended up eating four. I want to convey my regret
over what has happened. Which form should I use? A play would be an overkill.
I am not a big fan of play anyway. Same goes with a long novel. Crime and
Punishment by Fyodor Dostoevsky is also about regret, but that's a really
different kind of regret from my rather silly regret over binge eating. I think
poem, short story, and essay would all be good choices. I decide to go for
a poem because I want to make it short and impactful.

After deciding the genre, I consider which format I would like to use.
A sonnet? It would take more time and effort than I would like to spend.
A haiku? It could work. Or something outside such traditional formats? A poem
that I really liked pops into my mind: <a
href="https://www.poets.org/poetsorg/poem/just-say">This Is Just To Say</a> by
William Carlos Williams. The nonchalant cheerfulness of that poem fascinated
me, so I decide to try writing something similar. 

For software, let's say I want to develop something that would help people
learn how to speak English. Embedded software or shell scripts would not be
a good fit for this kind of software. Web application, desktop application, and
mobile application all seem to be solid choices. My application would be
something fairly simple, so I do not think it would require the power of
a desktop application. After some thoughts, I decide that having a microphone
and a speaker would be a crucial requirement, so I choose mobile application.
After all, all smart phones come equipped with both. 

For my mobile application, I have yet more decisions to make. Should I make
a hybrid application, or a native application? Their respective strengths and
weaknesses are too long to consider in this post. I choose native simply
because I want to use more of Apple's new Swift language. I need a server too.
I'm familiar with Ruby frameworks, and they work well, but I decide to go with
Swift all the way and try one of the new Swift backends. My application
wouldn't need anything serious from the backend, so I am okay with
experimenting. <a href="http://qutheory.io">Vapor framework</a> seems to
provide the most thorough documentation and most concise syntax, so I decide to
go with it. 

### Write, revise, write, refactor, ... 

I experiment with some ideas.

I took a cookie  
then another  
and then another  
and then another  
and I shouldn't have  

Well, that's silly - I'll at least give it that. I try something else. 

This empty cookie jar  
once held four cookies  
but they all decided to  
move into my stomach  

That wasn't a healthy decision  
either for them or me

It's still silly. I'm not quite happy with it. I start to get convinced that
everything I write will be equally disastrous. I keep writing it again and
again, experimenting with different words and structures. I keep doing it until
I feel like "Screw this! I've tried enough!" or until I reach a deadline for
publishing it. 

What about software development? Let's say in the process of coding, I found
out that I need to multiply an array of integers by 10. I have multiple
approaches to achieve that. First I decide to go with the old-fashioned C-style
loop.

{% highlight swift linenos %}
let applicationUsageCounts = [3, 8, 9, 11, 2]
var multipliedApplicationUsageCounts = [Int]()
for var i = 0; i < applicationUsageCounts.count; i++ {
  let j = applicationUsageCounts[i] * 10
  multipliedApplicationUsageCounts.append(j)
}
multipliedApplicationUsageCounts // [30, 80, 90, 110, 20]
{% endhighlight %}

It does its job. But C-style loop goes against Swift's coding style. Moreover,
I heard that C-style loop and ++ operator will be both deprecated in the
upcoming Swift 3.0 version. So I get to cleaning and improving my code.

{% highlight swift linenos %}
var multipliedApplicationUsageCounts = [Int]()
for count in applicationUsageCounts {
  multipliedApplicationUsageCounts.append(count * 10)
}
multipliedApplicationUsageCounts // [30, 80, 90, 110, 20]
{% endhighlight %}

This is a fairly standard iteration over an array. It does what I need it to
do. But then I discover another case where I need to multiply an array of
integers, this time with a different multiplier. I extract that iteration and
multiplication part into a function. While I am at it, I decide to use a more
functional programming style of code by using the map method. 

{% highlight swift linenos %}
func multiplyArray(array: [Int], withMultiplier: Int) -> [Int] {
    return array.map( { $0 * withMultiplier } )
}
let result = multiplyArray(applicationUsageCounts, withMultiplier: 10) // [30, 80, 90,
    110, 20]
{% endhighlight %}

I wonder if I should modify that function to also support an array of rational
numbers, not just integers, but decide to wait until I really need that kind of
generic function. I move onto the next part of my application, then repeat this
process of writing code and changing and improving it. 

### Conclusion

I hope that this comparison provided some insight into certain similarities
between writing and coding. Of course, such metaphor should not be taken too
far outside its intended use. It wouldn't be particularly useful to consider
cadence in software codes or to apply class inheritance to creative writing. In
the next post, I will try to explore other areas where comparing writing and
coding could raise some intersting questions.
