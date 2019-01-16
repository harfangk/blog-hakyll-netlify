---
title: Quora Q&A Session With David Heinemeier Hansson (Jan 25, 2017)
---

This is a copy of a Q&A session with David Heinemeier Hansson hosted by Quora on January 25, 2017. [Here](https://www.quora.com/session/David-Heinemeier-Hansson/1) is the link to the original Quora site. All questions are posted by Quora users and answered by DHH.

<!--more-->

## What makes Rails a framework worth learning in 2017?

The same reasons why it was a framework worth learning in 2004. The more things change, the more they stay the same. While we’ve seen a lot a progress in the JavaScript world, we’ve also seen a regression to the complexity-laden world that Rails offered refuge from in the early days.

Back then the complexity merchant of choice was J2EE, but the complaints are uncannily similar to those leveled against JavaScript today. That people spent hours, if not days, just setting up the skeletons. The basic build configurations. Assembling and cherry-picking from lots of little libraries and frameworks to put together their own snowflake house variety.

The core premise of Rails remains in many ways as controversial today as it was when it premiered. That by formalizing conventions, eliminating valueless choices, and offering a full-stack framework that provides great defaults for anyone who wants to create a complete application, we can make dramatic strides of productivity.

It’s somewhat surprising to me that despite the astounding success of Rails, that there haven’t been more competition for this proposition. The vast majority of activity today is for yet another option on the a la carte menu. Yet another build system, yet another view library, yet another ORM. Very little activity in integrated solutions.

I guess the answer is that the foundational proposition of Rails continues to cut against the psychological grain of most programmers. That by reducing choices and accepting community conventions and answers to most of the basic questions in web development, you end up better off. Less unique, less tailored, but in ways that just don’t matter anyway.

Anyway, that’s the big ideological appeal of Rails. I’ve elaborated further on convention over configuration, the a la carte/omakase conflict, the appeal of integrated systems, and other core values of the Rails community in [The Rails Doctrine](http://rubyonrails.org/doctrine/).

After reading that, you’ll probably have a pretty good idea as whether Rails is something for you or not. If you can’t recognize any of the struggles outlined in that document, or you just don’t like the solutions presented to those struggles, the particulars of Rails technology probably doesn’t matter much. If that document resonates, or at least piques your interest, read on.

On top of these ideological choices, we’ve built an incredibly pragmatic and multi-paradigm web framework. When people hear “web framework”, they sometimes thinks that “oh, that’s just some stuff to generate HTML, right?”. And in that definition, some might see it as though Rails competes against something like React. And I suppose it does, but in a very remote way that isn’t very useful to thinking about whether Rails is right for you or not.

As I talked about above, Rails has an incredibly ambitious mission. In the full-stack goal lies a pursuit to deal with just about every piece of code needed to connect databases and no-sql stores to a business domain model written in Ruby to a set of controllers that expose that model via REST and then, yes, finally to HTML. But that last step is a small minority of the code and focus of Rails.

So if you think that client-side MVC, React, Angular, or whatever is The Future, then you’re still squarely in the target audience for using Rails. Because the bits you use to design your HTML/JavaScript-based UI still needs to connect to a back-end domain model that saves stuff to the databases, computes things, enqueues jobs for later processing, sends out emails, triggers push notifications, and all the other stuff that real apps need to do.

And that’s where the meat of Rails sits. In what happens once that POST or PUT or GET is triggered. Now, as I said, Rails is full-stack by default. So of course we also include answers for how to generate and update HTML. We have some phenomenally productive answers in Turbolinks and SJR, but even if that path doesn’t appeal, everything that leads up to generating that JSON is still stuff we’ll have in common.

Anyway. That’s a very long pitch for two basic tenets of Rails appeal in 2017: 1) We have a unique ideological foundation that’s still controversial today and offers the same benefits against the mainstream choices as it did 13 years ago, 2) We have a pragmatic, full-stack answer that could be formulated based on that ideology that still offers amazing productivity from the second you run the rails new command.

Oh, and on top of all that, I’ve saved the cherry for last. You get to use Ruby, which, even in a world that has rediscovered the benefits of functional programming and immutability, remains the most extraordinarily beautiful and luxurious language I’ve yet to encounter. Just look at some code. I dare you not to fall in love.

## What is the future of web development in your opinion?

For the next five years, I think yesterday’s weather is the best guide. More of the same. Better APIs that conquer more of the territory that native has had to itself for a while, while retaining the cross-platform, productive, free-platform appeal of the web. That has remained a winning strategy for many/most software users, even at the time when the split between native and web performance was the greatest. And now it’s smaller than its ever been.

Have you used the web on an iPhone 7? It’s fucking amazing. I must admit that when we saw what the JavaScript performance looked like in 2012–2014, it was fair to have some worries over whether the open web would be able to stand a chance against the walled mobile gardens. But when you look at just how insanely much faster and better we’ve gotten in just the last 3–5 years, it’s never been easier to have hope and to invest in that hope.

Beyond five years? Who the hell knows.

## Will JavaScript frameworks take over Rails?

“Take over” assumes that Rails had anything to steal. It doesn’t. That’s the beauty of working with and for the web. The diversity and freedom of implementation choices behind the HTTP answer is infinite. So it’s not a zero-sum platform war in the sense of Android vs iOS or whatever other technological battlefield people have been indoctrinated to view the world as.

Now, you can of course think of “take over” as in “carry the current hype torch”. And the answer to that question is currently a resounding yes, and THAT’S GREAT! We hype what is new, and Rails is not new. Rails is 13 years old, which is the technological equivalent of a tribal elder at this point. It would be sad if Rails was still the focus of hype. Thankfully it is not.

Which leaves us free to pursue other and greater pleasures. Rails is now mainstream enough that it’s a safe choice for people who really need safe choices. It’s no longer the wild west. It’s not frontier country. You can suggest building your next app at the bank in Rails and people will nod about the risk profile of that. Oh how far we’ve come!

But such progress doesn’t come without giving up some sheen. You can’t both the hottest new thing in town and the safe choice for millions of programmers. Those two don’t overlap on the venn diagram.

I for one am not one to look back longingly for the days past. I don’t miss my high school days. I don’t miss my twenties. I won’t miss my thirties. I revel in moving through life’s phases as they happen. Embrace the now, embrace who you’ve become, don’t linger on who and what you were. I seek to do the same for Rails.

We have accepted an incredible honor to still be standing after 13 years. And not just standing, but continuously improving and offering that controversial ideology I presented in the first answer. The world of programming is so much richer because we’re in it. I say that braggingly proud because, well, damn, I AM PROUD. What an achievement for a community and a mission that started out at the first step on Gandhis ladder of FIRST THE IGNORE YOU, THEN THEY LAUGH AT YOU, THEN THEY FIGHT YOU, AND THEN YOU WIN. We’ve won.

## If you were starting out fresh today and could tap into one market, what would it be and why?

Making paid software for small and mid sized businesses remains the most appealing risk/reward profile as far as I’m concerned, so I’d probably take another crack at that from a different angle. There’s an infinite number of problems to solve for this group of companies. Maybe not all of them as lucrative and expansive as Basecamp, but so what. If I had succeeded even to the tune of 1/10th of what we’ve accomplished with Basecamp, I would be living a very wealthy, fulfilled life.

That and I just love making business software. I know, it’s weird. It doesn’t have any of the glamour of consumer software, but that’s where my heart lies. I love making things more efficient, easier to use, and ultimately contribute to the (hopefully just) 8 hours a day that most people spend at work.

That and there’s just SO MUCH BAD BUSINESS SOFTWARE! It offends me deeply, and thus motivates me equally so to rectify at least some corner of that.

## Is Rails going to be relevant in 5 years?

Yes.

## How has the SaaS business changed in the last ten years?

When we started, you had to worry about lots of trite basics. Like, will businesses actually pay on a subscription basis for software? Sorta an unproven hypothesis at the time. And along with that came all the risk-averseness from banks and credit card processors. All that pain and uncertainty is just gone. Poof.

So things have gotten so much easier on the commercial and technical side of things. Already, when we started 15 years ago, it was pretty easy and cheap to get going. Now it’s so much easier and cheaper (= free) to get going. It’s a marvelous time to be a SaaS entrepreneur!

What hasn’t changed, though, is that you still need to build great software and sell it at a reasonable price. And you have to build an audience who’ll care. Build It And They Will Come wasn’t true then and it isn’t true now. And the work to overcome that is for marathoners, not sprinters.

## Did you talk to Matz about Ruby 3 progress, and how is it going to affect the future of Rails as a framework?

[Ruby has been fast enough for 13 years](https://m.signalvnoise.com/ruby-has-been-fast-enough-for-13-years-afff4a54abc7), so I view any additional progress on the speed of Ruby as just free gifts. I don’t expect them, I don’t ask for them, I don’t even follow their development THAT closely, but I say THANK YOU VERY MUCH when they arrive at my door steep. It just isn’t something I really spend a lot of time thinking about.

Computing power is so much greater and so much greater than it was when we got started. And back then it was already PLENTY!

Now, that’s for a business model like Basecamp. If your business model is to make pennies off millions of users by selling their privacy or eyeballs, you may well have a different cost structure. One that doesn’t allow for such a luxurious language as Ruby. That’s fine. Ruby doesn’t have to be for every use case.

## What are some of the biggest challenges remote teams face in 2017 and what solutions are available?

Too much technology. It used to be that remote was hampered by technology not being good enough, now it’s the opposite. Many remote-work environments are turning into the digital equivalents of the open-plan office: A hell for focus and deep thought, constantly bombarded by notifications and real-time presence oppression. It’s as sad as its ironic.

Jason, my business partner and coauthor, wrote up a [comprehensive treatise on this problem as it relates to group chat](https://m.signalvnoise.com/is-group-chat-making-you-sweat-744659addf7d). That’s a good place to start on this.

But it’s more than just that. I recently saw a write-up from somehow who said their small team of 28 people were using 33 different apps at the office to organize and communicate and run the business. Holy fuck. They were using Trello, Asana, Basecamp, Slack, Facebook At Work, and million other things. Good luck trying to decide where to start a discussion or find anything in that.

Yet I’m sure they’re not unique. Everyone is so worried that they’re going to miss out on The Next Big Thing that they just do everything all the time. That’s just sad, and completely unnecessary.

Anyway, I wrote a whole book on the topic in [REMOTE](http://37signals.com/remote). Check it out.

## What do you think of Elixir and Phoenix in comparison to Ruby and Rails?

Love that Ruby’s lessons have served as inspiration in elixir. And I love the idea of picking up something old, like Erlang, and saying, hey, this is actually pretty great!

But I don’t personally have a use case where the Elixir/Erlang/Phoenix combo offers something that’s compelling enough to warrant the downgrade from using Ruby. I wouldn’t want to write, say, Basecamp in such a pairing, and, hey, Basecamp is what I write.

I’d encourage others to check it out, though. It seems like a great solution in particular if you need to deal with millions of users connected with persistent connections and each user isn’t very valuable. I can totally see why WhatsApp picked Erlang, and that’s a great use case for something that wouldn’t be a good fit for Ruby.

So. Big fan. But just not something I personally need.

## Why did David Heinemeier Hansson choose Ruby (over Python?) to build Rails?

Why do people fall in love? You can break it down to high cheek bones or shared interests or a dangerous date or any other parts of the puzzle, but it’ll in large parts remain a puzzle. Not because you don’t know what the pieces are, but because you can’t foresee the way they’ll fit together for you.

That’s Ruby for me. I’m in love with Ruby and has been for the past 14 years. I could pull out the blocks, the aliased-for-the-perfect-occasion keywords, the open core classes, the consistency of the OOP while accommodating the peaches of procedural and functional programming. But these are all pieces, and many other languages share some of the pieces put together in different ways. Yet Ruby is the one I love.

I’m sure plenty of programmers will find so vague, emotional language silly if not downright annoying. There’s a large class of programmers who think that because we occasionally intersect with a field that has the word science in it, that we’re supposed to be these rational automatrons that just pick the objectively BEST TOOL FOR THE JOB.

I think that’s a bag of bollocks. There are no BEST TOOLS. There are only puzzles that tickle your brain just right. Almost everything today can be made to make anything. That’s glorious. Viva diversity of expression, language, and thought.

## What three books have had the greatest impact on your life?

1. [Influence](https://www.amazon.com/Influence-Psychology-Persuasion-Robert-Cialdini/dp/006124189X/ref=sr_1_1?ie=UTF8&qid=1485276322&sr=8-1&keywords=influence+the+psychology+of+persuasion). It taught me so much about marketing and human psychology. I keep referring back to that book all the time. It has been instrumental in inspiring the marketing strategies I used to propel Rails and Basecamp.

2. [Smalltalk Best Practices](https://www.amazon.com/Smalltalk-Best-Practice-Patterns-Kent/dp/013476904X). My favorite book about programming. It’s more than twenty years old now, but it remains as relevant as ever. It captures so many of the patterns I love to dance with when writing beautiful code.

3. [Meditations](https://www.amazon.com/Meditations-Thrift-Editions-Marcus-Aurelius/dp/048629823X/ref=sr_1_1?s=books&ie=UTF8&qid=1485276427&sr=1-1&keywords=marcus+aurelius+meditations). Marcus Aurelius is probably my favorite Stoic philosopher, with Seneca as a close second. And in Stoic philosophy I’ve found kindred thought for mental coping mechanisms I had employed since childhood. It refined them further and cemented their place as the guiding philosophy of life for me.

Now these are more like 3 favorite books that just came to mind when trying to answer quickly. I read about 10–12 books per year, so I’m sure a rigorous examination of them all may well have produced a different list. But these are good books!

## What is the most productive programming language in your opinion other than Ruby?

I’ve really come to like JavaScript. No, it’s not Ruby, but then again, nothing is. ES2015 was a huge step forward. Particularly the class notation and the metaprogramming improvements. And now it seems like there’s finally good momentum to keep improving.

I’m less enthusiastic about the JavaScript ecosystem, but as a language, yes please. It shares many of the values of Ruby. Open prototypes, metaprogramming at the forefront, etc.

## What startups do you think will make it big in 2017 and which are destined to tumble?

If you’re thinking of the VC startup circus, I’ll give you an answer you can use for any year: Almost all of them will stumble and fail. A tiny, tiny minority will make it big. A few will take a job and call it an acquisition of THEIR INCREDIBLE JOURNEY.

## How do you keep learning?

I read a fair amount. I’m frequently trying to benchmark the opinions I have against alternatives to see if they still hold. This is particularly so for technology, but applies broadly as well. It’s exhilarating to try to invalidate your current operating thesis, because if you can, there’s a leap to make. I’ve made many such leaps.

Beyond that, I’m a big believer in diligent practice. It doesn’t help to practice the same lesson 10 times over. You have to constantly seek the edge of your comfort zone. That gets harder and harder as you get more proficient, because the drop feels deeper when you step into the unknown, once you’re an expert somewhere else. All the more reason to keep at it, lest you stagnate.

I also try my best to be cross disciplinary. You can learn a lot about open source community building by studying the history of nation states and political processes. You can become a better programmer by becoming a better writer and philosopher. And really interesting things happen when you try to apply the best learnings from one domain to another.
