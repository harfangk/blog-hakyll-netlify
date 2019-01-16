---
title: Sandi Metz's Advices for Better Code
---

I've recently finished reading the
Practical Object-Oriented Design in Ruby by Sandi Metz. I heard a lot of good
things about the book and it was definitely worth reading - I highly recommend
it too!

<!--more-->

I've also listened to Ruby Rogues podcast episode 87 where the panelists
discusses the book with Sandi, and it also has some great discussions. Here are
some "rules" for development from the podcast by Sandi.

1. Your class can be no longer than 100 lines of code.
2. Your methods can be no longer than 5 lines of code. 
3. You can pass no more than 4 parameters and you can't just make it one big
   hash.
4. In your Rails controller, you can only instantiate one object, to do
   whatever it is that needs to be done.
5. Your Rails view can know about only one instance variable.

One thing I'd like to point out is that Sandi didn't mean these rules as
unchangeable rules. They are more like some guidelines to think about
when we write code, and if we can explain why they should not be applied, we
can ignore them.

I don't think there are many cases that'd justify breaking free from rule 1,
4 and 5. It will be harder to stick to rule 2, especially when it's early in
development and there's not enough information to determine which features should be implemented.

I'm a bit concerned about rule 3. That recommendation could mislead people to
think that it's okay to have methods 4 parameters all the time. I don't know
what exactly Sandi meant by that rule, but I'm definitely not comfortable with
methods with 4 parameters. They are hard to understand. My personal ceiling is
2 parameters with 1 optional hash. Any more than that calls for refactoring.
