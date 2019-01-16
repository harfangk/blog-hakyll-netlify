---
title: Educational Potential of Languages and Frameworks
---

Just like most of new Ruby on Rails developers, I ended up creating a Lovecraftian nightmare of huge tangled models in my first project. It'd have been nice to have an experienced developer to prevent me from making that mess. Unfortunately, I didn't have one. I had to figure a way out of it on my own.

Reading through numerous blog posts, I found out that it was not just me. This problem was caused by the way Rails used Active Record pattern, and was prevalent among many Rails developers. I could even see some kind of historical eras of Rails application structure: the Fat Controller Age, the Fat Model Age, and the Plain Old Ruby Object Age. There were other solutions too. For example, Ruby Object Mapper and Hanami framework used Data Mapper pattern to replace Active Record pattern entirely. 

When many people end up making the same mistakes, it can no longer be attributed to the individuals. That's a sign that there's a design flaw in Rails that repeatedly misleads people into making those mistakes. But I'm fine with the fact that it has flaws. After all, there's no such thing as a perpetually perfect system, and Rails got far more things right than wrong. 

<!--more-->

### Unfortunately, design flaws don't get fixed more often than we'd like

But I'm not fine with a design flaw never getting fixed, especially when there already exist solutions for them. The way Rails uses Active Record is an example of such a design flaw that's never getting fixed. Look at this section about Active Record pattern in Clean Code by Robert C. Martin.

> Active Records are special forms of DTOs [Data Transfer Object]. They are data structures with public variables; but they typically have navigational methods like save and find. Typically these Active Records are direct translations from database tables, or other data sources.  

> Unfortunately we often find that developers try to treat these data structures as though they were objects by putting business rule methods in them. This is awkward because it creates a hybrid between a data structure and an object.

> The solution, of course, is to treat the ActiveRecord as a data structure and to create separate objects that contain the business rules and that hide their internal data (which are probably just instances of the ActiveRecord).

That book was written in 2008 and is quite famous among programmers. I do know that Rails was initially released in 2004, way before that book. But I'm sure that most of experienced developers already knew of the pitfalls of Active Record pattern well before the book was published anyway. And Rails core team would definitely belong to that group of experienced developers.

And it's 2016, and Rails still misleads developers into mixing domain logic with persistence logic in its models. In those 12 years Rails went through four major version updates in which it could have dealt with that fundamental design flaw. But it never did. All those lessons we learned through experience are wasted if the system does not incorporate that feedback.

Rails is just one example. It seems that this pattern is found throughout the field of software development. I remember panelists in Ruby Rogues podcast talking about the same frustration when they talked about the book Structure and Interpretation of Computer Programs. They lamented that the book was published in 1979 and contains answers to many of the problems that we continue to see even today. But what good do those solutions do when they never get implemented in the field?

### There's a systematic bleeding of practical knowledge in this field

I think there's a systematic bleeding of practical knowledge in this field. That's not surprising when there's actually no system to pass down that knowledge. Computer science curriculua rarely stoop down to get in contact with field level knowledge. Software engineering curricula lack shared principles and topics needed to form a common ground for the field. As experienced developers retire and old companies dissolve, so vanishes the wealth of accumulated practical knowledge.

This issue is exacerbated by the fact that practical knowledge in software development is [tacit knowledge](https://www.wikiwand.com/en/Tacit_knowledge) that's hard to transfer to another person by writing or telling. For example, you can't learn how to drive just by reading explanations on it. Driving a real car is the only way to learn how to drive, at least for now.

But it's definitely possible to nudge drivers into adopting better driving habits while they're learning how to drive. Sensors to measure distance with the car in front of you will help you maintain safe distance with that car. Camera to show the rear view will help you drive in reverse gear. Clear and wide road surface markings will help you stay in lane. 

Fortunately, such development is already happening in software development thanks to the explosive growth of open source software. New programming languages incorporate the solutions to common problems in the industry at the language level. New frameworks tinker with different combinations of various development principles to make development more productive and enjoyable. 

### Languages and frameworks could serve educational role

But I think the drive behind such effort usually just stops at providing better productivity and less stress at work. I hope more thoughts could be given to how new developers would use them and what they would learn when using them. In other words, the educational value of such systems. If just using a particular language or framework could help new developers develop good practices and habits, it would save our industry as a whole a lot of time and money spent on training them. 

For example, if Rails promoted the separation of domain logic and persistence logic at the framework design level, new developers would have internalized that practice. That would have prevented all this unnecessary cost and pain of fixing the damage caused by bad coding habits and mental models that new developers learned not knowing a better way.

I'm not saying that educational value should be the main consideration when developing new systems. That'd be putting the cart before the horse. But we should carefully observe how new developers are using the system  and implement appropriate mechanisms to help them develop better habits. That might sound like an unnecessary extra work, but I believe that it would make everyone's lives much easier in the end.
