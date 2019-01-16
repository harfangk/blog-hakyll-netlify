---
title: Why I Chose Docker for Sandboxing
---

## Overview

I wanted to build an Elixir application that can run Elixir code provided by users and return the results. Maintaining security is critical for an application like this, since I could be running some evil codes written by diabolical mastermind residing in the scary trench of Internet. I've never done something like this before, so I had to research extensively for a good solution.

Unfortunately there does not seem to be a perfect solution, as always. After considering tradeoffs, I chose to settle for Docker container with some security options. I think it's a reasonably secure and reasonably performant option for my use case. This blog post is a journal of what I've learned while building the application.

I am by no means an expert in this matter, so read this as not as a survey of technology but as a personal journal. I do welcome correction and additional resources to better my understanding. And if you want an overview of sandboxing technology landscape, I recommend [Sandboxing landscape](https://idea.popcount.org/2017-03-28-sandboxing-lanscape/) by Marek (@majek04). It's a broad, up-to-date, and also well-written article as much as I can tell.

<!--more-->

## What I Am Building

I am building an Elixir learning site where users can solve practice problems in the middle of lesson texts. There are already a lot of tutorials and interactive REPLs on web browser out there, so I thought this would be a technically trivial application. Well, it seems that I was wrong. (Note to self: never trust someone, even myself, who says something is 'trivial' or 'simple'.)

The technical challenge was in providing a sandbox environment where *any* code could be safely compiled and run. After all, I did not want my server to explode from a fork bomb or get hijacked into a cracker's servitude. There were several approaches I've considered, but they all turned out to be unsatisfactory.

## Code Level Approach

First idea was to blacklist potentially dangerous codes. These include functions that could affect the Erlang virtual machine or the operation system itself. I could traverse the abstract syntax tree of the code submitted by users to detect blacklisted functions. But I quickly realized that this was a hopeless endeavor. It's practically impossible to blacklist all possible ways of defining, loading, and calling functions when potential attackers can always come up with a creative workaround.

The next idea was to whitelist safe codes. But this ran against my goal of allowing users to try running any Elixir code as they learn to program. Moreover, quickly scanning the source code of the language itself revealed that many of core language functions depended on some functions that could be potentially abused. This approach would have required me to fork Elixir to create a version suited to my use case. I have neither capability nor willingness to do such a thing.

## OS Process Level Approach

More reasonable approach seemed to go with OS process level sandboxing. My initial choice was to use Docker container as a sandbox. I've seen some people do it, so thought it would work. Some more research revealed, however, that Docker is not so secure after all. Solving sandboxing problem is not the goal of Docker, so while it provides some reasonable security, it can be breached by someone dedicated enough under certain situations.

There were a lot of discussions about how that could happen. Honestly, I didn't really understand much of it. Still, I did learn that no practically useful sandbox can be perfectly secure. A perfectly isolated sandbox is actually perfectly secure, but then it has no use because there's no way to work with it.

By principle the more a sandbox is isolated from the host system, the more secure it becomes. This means that a full virtual machine is likely to be more secure than a Docker container, since the latter shares kernel with the host system while the former runs its own kernel on top of the host system's kernel. So should I go with a complete virtual machine?

Unfortunately a full-blown virtual machine consumes much more resource than Docker. Indeed, relative resource efficiency is one of the selling points of Docker after all. So with the same amount of hardware capacity, I could serve many more users if I used Docker. I had to make a tradeoff between security and throughput.

But once I realized that my application had to create a new sandbox environment whenever it ran the code submitted by users, I decided that resource efficiency was the bigger factor here. Since I decided not to limit what kind of code users could run, there could be code that affected the sandbox system or Erlang VM running on it. The only way to guaranteed that each code was compiled and run independent of one another was to provide a new sandbox each time. Obviously this is a very resource intensive operation. I first implemented it with Docker and the slowdown in running code was already significant. If implementation using Docker was this slow, then one using a full VM would be unacceptably slow. So I decided to settle for Docker for now.

## Interim Thoughts

I never knew that sandboxing was a such a difficult technological problem. In fact, after reading and thinking through this issue, I think running any arbitrary code while still guaranteeing security is theoretically impossible; although I can't prove it, I can tell that the formulation of this problem is very similar to undecidable problems in computer science. In order to guarantee security, the power of program should be limited in one way or another. By using Docker container as a sandbox, I chose not to limit the power of program within the sandbox but limit its power to affect outside world.

I chose to use Docker because I think it fits my use case. With proper configuration, it provided reasonable security and reasonable performance. My application is trivial enough to risk some potential security vulnerability. But if I had to implement sandbox in a critical system, I will have to choose a different option. At least I learned that much through this endeavor.

## Potential Solution: SmartOS Zones

An interesting suggestion was to use Zones technology provided by SmartOS, an operation system based on Solaris, as it was built with virtualization and security in mind. I will definitely have to look into this if I want a more serious sandbox.
