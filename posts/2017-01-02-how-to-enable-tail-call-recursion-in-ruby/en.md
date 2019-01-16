---
title: How to Enable Tail Call Optimization in Ruby
---

This post assumes basic understanding of recursion and Ruby. 

## What is tail call optimization

*Tail call optimization* (TCO) is an optimization strategy for tail-recursive procedures. It is useful in preventing stack overflow when using recursion because it limits the call stack size of a recursive procedure to one. 

<!--more-->

## How TCO works

Since TCO works by optimizing the size of call stack, let's start by talking about the call stack. A call stack is where a program stores the information about its active subroutines. 

When a procedure is called, the caller pushes the return address of that procedure onto the call stack. And when the procedure finishes, the caller pops the return address of the procdeure from the call stack and returns to that address. When it does so, it brings along the return
value of the previously called procedure. Then it continues running the main program. 

Each call of a procedure adds a new stack frame to the call stack. The stack trace we see when debugging an error represents the call stack leading up to the stack frame where the error occurred. When the number of stack frames goes beyond the allowed size of call stack, stack overflow happens and the program crashes. 

That's enough for a brief description of the call stack.

Let's move onto what a tail call means. A tail call is a subroutine that is called as the final action of a procedure. In other words, it's the last thing
called in a procedure before it ends. This implies that most of the stack frame of the procedure that are used before the tail call can be discarded and replaced by the new stack frame from the tail call. This results in improved performance in terms of memory. Replacing existing stack frame like that instead of adding a new one is called *tail call elimination*. When a program does tail call elimination, it is said to perform *tail call optimization*.

For non-recursive procedures, TCO saves little space or time and is unimportant. For recursive procedures, TCO can make or break the program.

## Why is TCO important for recursion

When a procedure is called recursively, each call adds a new stack frame onto the call stack, which can quickly grow unmanageable and cause stack overflow. TCO limits the number of stack frames from all the calls of a recursive procedure to one stack frame. In other words, when a recursive procedure is called n times, it requires O(n) call stack size. TCO reduces that to O(1).

It cannot work on all recursive procedures, though. It requires that the procedure is *tail-recursive*, which means that a procedure calls itself as its own tail call.

## Warning about using TCO in Ruby

TCO is neither required nor prohibited in Ruby. Some implementations, including YARV and Rubinius, support it, while others, such as JRuby, do not. So when you use a recursive procedure that relies on TCO, know that it will not work on JRuby. 

## Factorial code

{% highlight ruby %}
class Fact
  def self.iterator(n)
    (1..n).reduce(:*)
  end

  def self.non_tail_recursive(n)
    1 if n <= 1 
    n * non_tail_recursive(n - 1)
  end

  def self.tail_recursive(n, acc = 1)
    return acc if n <= 1
    tail_recursive(n - 1, acc * n)
  end
end 
{% endhighlight %}

We will use factorial to test tail recursion. We have three versions - iterator, tail-recursive, and non-tail-recursive. 

Let's try running them.

{% highlight ruby %}
puts 'Iterator result'
=> Iterator result
puts Fact.iterator(100000).to_f
=> Infinity

puts 'Non-tail-recursive result'
=> Tail-recursive result
puts Fact.non_tail_recursive(100000).to_f
=> fact.rb:6:in `non_tail_recursive': stack level too deep (SystemStackError) `

puts 'Tail-recursive result'
=> Tail-recursive result
puts Fact.tail_recursive(100000).to_f
=> fact.rb:11:in `tail_recursive': stack level too deep (SystemStackError) `
{% endhighlight %}

Only the iterator version works. Both recursive versions give `SystemStackError`. Let's try it again with TCO enabled. 

## How to enable TCO in Ruby

{% highlight ruby %}
RubyVM::InstructionSequence.compile_option = {
  :tailcall_optimization => true,
  :trace_instruction => false
}
{% endhighlight %}

You can set the `compile_option` in `RubyVM::InstructionSequence` to enable TCO. That looks simple but there's a complication here. Change to `RubyVM` happens at runtime, so you can't make it work by just including those lines to the file. There are two ways to make it work. 

{% highlight ruby %}
RubyVM::InstructionSequence.compile_option = {
  :tailcall_optimization => true,
  :trace_instruction => false
}

class Fact
  def self.iterator(n)
    (1..n).reduce(:*)
  end

  eval <<END
    def self.non_tail_recursive(n)
      1 if n <= 1 
      n * non_tail_recursive(n - 1)
    end

    def self.tail_recursive(n, acc = 1)
      return acc if n <= 1
      tail_recursive(n - 1, acc * n)
    end
  END
end 
{% endhighlight %}

This is the first option. You use `eval` to evaluate the method definition at runtime, overriding the method definition that was parsed at compile time.

{% highlight ruby %}
# fact.rb
class Fact
  def self.iterator(n)
    (1..n).reduce(:*)
  end

  def self.non_tail_recursive(n)
    1 if n <= 1 
    n * non_tail_recursive(n - 1)
  end

  def self.tail_recursive(n, acc = 1)
    return acc if n <= 1
    tail_recursive(n - 1, acc * n)
  end
end 

# ruby_vm_option.rb
RubyVM::InstructionSequence.compile_option = {
  :tailcall_optimization => true,
  :trace_instruction => false
}

require_relative 'fact.rb'
{% endhighlight %}

This is the second option. TCO is enabled in `ruby_vm_option.rb`, enabling TCO in all subsequently loaded files including `fact.rb`.

Let's try running it again with TCO.

{% highlight ruby %}
puts 'Iterator result'
=> Iterator result
puts Fact.iterator(100000).to_f
=> Infinity

puts 'Non-tail-recursive result'
=> Tail-recursive result
puts Fact.non_tail_recursive(100000).to_f
=> fact.rb:6:in `non_tail_recursive': stack level too deep (SystemStackError) `

puts 'Tail-recursive result'
=> Tail-recursive result
puts Fact.tail_recursive(100000).to_f
=> Infinity
{% endhighlight %}

This time the tail-recursive one works, too. But the non-tail-recursive one still gives `SystemStackError`.

## Conclusion

We've briefly looked at what is tail call optimization and how to enable it in Ruby. It is definitely possible for default YARV interpreter and Rubinius, but is not supported by JRuby because JVM does not support TCO. It's an interesting tool to use, but know its limitations before using it!
