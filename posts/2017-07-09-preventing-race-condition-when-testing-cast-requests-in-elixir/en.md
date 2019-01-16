---
title: Preventing Race Condition When Testing Cast Requests in Elixir
---

This post assumes understanding of how `GenServer.cast/2` and `GenServer.call/2` work.

## TL;DR

Call a `GenServer.call/2` function after `GenServer.cast/2`. This prevents your caller process from executing any more code until it receives a reply to the `call/2` function from the receiver process. The receiver process handles and sends the reply to `call/2` only after it has handled the message from `cast/2`. This can ensure sequential execution of code. If you need a generic function, `:sys.get_state/1` can be used for this purpose.

<!--more-->

## Initial Solution

I discovered this issue when writing a simple test for `cast/2` function. The test calls the function and then makes an assertion about its expected result. It failed because of a classic race condition case: the assertion was made before the `cast/2` function was handled by the receiver process.

I had to find a way to ensure that the assertion is made only after `cast/2` was handled. The crudest solution was to call `Process.sleep/1` to wait for a specified amount of time between `cast/2` and `ExUnit.Assertions.assert/1`. This did make the test pass, but I wanted to write something better than that.

## Better Solution

The next solution I found was to call `call/2` between `cast/2` and `assert/1`. This makes use of how BEAM processes and their mailboxes work, and how `call/2` works.

A process mailbox concurrently receives messages sent by other processes, but sequentially handles those messages. So a mailbox serves as a sort of synchronizing point for messages. For example, if a process receives messages A and B in that order, it will handle A first and then handle B.

Now remember that `call/2` suspends the caller process until it receives the reply from the receiver process. Calling `call/2` after `cast/2` ensures that any code following `call/2` will be executed only after the caller process receives the reply sent by the receiver process. And the receiver process will handle the message from `cast/2` first, and then handle the message from `call/2`, then send the reply. This creates an order of execution specific enough to prevent race condition.

You can see an example from my toy project [here](https://github.com/harfangk/url_shortener/blob/master/test/url_shortener_ets_cache_interface_test.exs). I was using [ETS (Erlang Term Storage)](http://erlang.org/doc/man/ets.html) as a cache, but needed a solution to a race condition because some functions for interacting with ETS were using `cast/2`.

## Avoid Race Condition in the First Place

But maybe there's an even better solution. If we can circumvent the whole race condition issue, we can get rid of the problem it causes. This can be done by just testing `handle_cast/2` that always accompanies `cast/2` function. Directly testing `handle_cast/2` spares us from having to deal with message passing among processes and race conditions that it causes. I believe that this is the best approach for unit tests.

Unfortunately, that's not always an option. Integration tests require testing interaction among multiple processes. In that case, it might be necessary to synchronize processes with `call/2` functions to simulate how they are expected to work in production. On the other hand, sometimes we don't have access to callback functions. In fact, most modules keep their callback functions private. In that case there is no other way but to test both the message passing and callback handling in a single test case.

## An Afterthought

By design, each BEAM process could be running on separate machines. This means that message passing among BEAM processes should be able to deal with classic network problems like availability, disconnection, unresponsiveness, and so on. But my tests take none of these into accounts - they assume that everything will be alright, which is never the case in the real world.

Is this okay? I guess so in this simple case, because the processes run within the same BEAM instance. But if I do write more advanced distributed software, I suspect I will have to write tests for potential network issues too.
