---
title: Basics of Ruby Method Dispatch System (Part 1)
---

In this post, I will explain the basics of how a method call works in Ruby. I'll assume that readers have some familiarity with Ruby language. 

The post is in two parts. The first part covers what you need to know about typical Ruby program. It covers: ancestors hierarchy, class inheritance, and module `include` and `prepend`. 

The second part digs into tools that are used less often. It covers: singleton method, singleton class, and class methods.

<!--more-->

## Overview
The key concept to understand is an object's **ancestors**. An object's ancestors include all classes and modules that it inherits from. 

When a method is called on an object, Ruby interpreter browses the object's class to find the definition of the method. If the definition is not found, the interpreter moves up the ancestors hierarchy and searches for the definition of that method. If the interpreter fails to find the definition of the method in the highest ancestor, it starts the next procedure.

The interpreter traverses the ancestors again, looking for the definition of `method_missing` method this time. If there's no `method_missing` defined in the ancestors, then it returns `NoMethodError`. 

To summarize in a list:

1. Search the class of the object for the definition of the method.
2. Continue moving up the ancestors hierarchy and repeat the search.
3. Search the class of the object for the definition of `method_missing`.
4. Traverse the ancestors hierarchy to find the definition of `method_missing`.
5. Return `NoMethodError`.

`method_missing` is an essential tool used in metaprogramming, allowing programmers to respond to undefined methods in runtime. But it's outside the scope of this post so I won't mention it again.

## Level 0: Basic Class

We'll first look at the simplest construction typically seen in Ruby to go over the foundation. This part will be the longest in this post, because we'll go over basic concepts and tools we'll use.

{% highlight ruby %}
class BasicClass
  def basic_class_method
  end
end

basic_class_instance = BasicClass.new
=> #<BasicClass:0x007fedd2261138>
{% endhighlight %}

We defined `BasicClass` with an instance method `basic_class_method`, then created a new instance of `BasicClass` called `basic_class_instance`.

{% highlight ruby %}
basic_class_instance.class
=> BasicClass

BasicClass.class
=> Class
{% endhighlight %}

`class` method is defined in `Class` in Ruby standard library. When called on an object, it returns the class of the object. As you can see, `basic_class_instance` is an instance of `BasicClass`. Nothing surprising here. 

But it's more interesting to see that `BasicClass` is an instance of `Class`. Every class you define in Ruby is an instance of `Class` class.

In Ruby, everything is an object, which can mean a lot of different things. But for this particular aspect, let's just think about how an object is constructed. In most OOP languages, an object is constructed as an instance of a class. But everything, including class, is an object in Ruby. So a class that you defined is constructed as an instance of a class called `Class`. If this concept is new to you, take some time to think about it. There's a bit of recursive thinking here.

{% highlight ruby %}
BasicClass.ancestors
=> [BasicClass, Object, Kernel, BasicObject]

BasicClass.class.ancestors
=> [Class, Module, Object, Kernel, BasicObject]

BasicClass.instance_method(:basic_class_method)
=> #<UnboundMethod: BasicClass#basic_class_method>
{% endhighlight %}

`ancestors` method is defined in `Module` class of Ruby standard library. It shows the ancestors of a class or a module.

`BasicClass` has three ancestors: `Object`, `Kernel`, `BasicObject`. All Ruby objects inherit from these three modules. 

`Class`, which is the class of `BasicClass`, has four ancestors: `Module`, `Object`, `Kernel`, `BasicObject`. Interestingly, `Class` is a subclass of `Module` in Ruby. It might look quite strange, but that's how Ruby implements it. 

`Class` has some special capabilities that `Module` does not, for example constructing an object. A discussion about `Class` and `Module` warrants a separate post, so I won't talk about it here. Let's move to the next tool.

{% highlight ruby %}
BasicClass.instance_method(:basic_class_method)
=> #<UnboundMethod: BasicClass#basic_class_method>
{% endhighlight %}

`instance_method` is defined in `Module`, which returns an `UnboundMethod` that represents the instance method given in its arguments. In our specific case, it means that `basic_class_method` is an `UnboundMethod` that is defined in `BasicClass` under the name of `basic_class_method`. The last part might seem redundant, but it's useful when dealing with aliased methods.

We will just use `instance_method` to see where the instance method is defined in the ancestors hierarchy, so I will not discuss what `UnboundMethod` means in this post.

## Level 1: Superclass and Subclass

In this part we will look at another typical structure: superclass and subclass. We'll just use the tools we introduced in the previous part to analyze the structure without introducing new concepts.

{% highlight ruby %}
class SuperClass
  def super_class_method
  end
end

class BasicClass < SuperClass
  def basic_class_method
  end
end
{% endhighlight %}

We created a new `SuperClass`, and had `BasicClass` inherit from it. Let's look at their ancestors.

{% highlight ruby %}
BasicClass.ancestors
=> [BasicClass, SuperClass, Object, Kernel, BasicObject]

SuperClass.ancestors
=> [SuperClass, Object, Kernel, BasicObject]
{% endhighlight %}

`BasicClass` has another ancestor called `SuperClass`. Nothing unexpected here.

Let's look at `instance_method` next.

{% highlight ruby %}
SuperClass.instance_method(:super_class_method)
=> #<UnboundMethod: SuperClass#super_class_method>

BasicClass.instance_method(:basic_class_method)
=> #<UnboundMethod: BasicClass#basic_class_method>

SuperClass.instance_method(:basic_class_method)
NameError: undefined method `basic_class_method' for class `SuperClass'

BasicClass.instance_method(:super_class_method)
=> #<UnboundMethod: BasicClass(SuperClass)#super_class_method>
{% endhighlight %}

There's nothing notable in the first three cases. `SuperClass` has `super_class_method`, `BasicClass` has `basic_class_method`, and `SuperClass` does not have `basic_class_method`. 

The last one provides some interesting information. It tells us that `BasicClass` has `super_class_method` as an instance method, but the method is defined in `SuperClass`. 

In terms of method dispatch system, it means that Ruby interpreter traversed up a level in the ancestors hierarchy to find the definition of the method.

## Level 2: Including and Prepending Modules

In Ruby, module mixin is used alongside class inheritance to organize data and functions and provide namespace. When a module is mixed in, its constants, methods, and module variables are appended to the receiving module. Let's see how module mixin is represented in ancestors hierarchy.

{% highlight ruby %}
module ModuleIncludedToBasicClass
  def method_included_to_basic_class
  end
  
  def defined_in
    puts 'This method is defined in ModuleIncludedToBasicClass'
    super
  end
end

module ModulePrependedToBasicClass
  def method_prepended_to_basic_class
  end
  
  def defined_in
    puts 'This method is defined in ModulePrependedToBasicClass'
    super
  end
end

module ModuleIncludedToSuperClass
  def method_included_to_super_class
  end
  
  def defined_in
    puts 'This method is defined in ModuleIncludedToSuperClass'
    super
  end
end

module ModulePrependedToSuperClass
  def method_prepdended_to_super_class
  end
  
  def defined_in
    puts 'This method is defined in ModulePrependedToSuperClass'
    super
  end
end

class SuperClass
  include ModuleIncludedToSuperClass
  prepend ModulePrependedToSuperClass
  def super_class_method
  end
  
  def defined_in
    puts 'This method is defined in SuperClass'
    super
  end
end

class BasicClass < SuperClass
  include ModuleIncludedToBasicClass
  prepend ModulePrependedToBasicClass
  def basic_class_method
  end
  
  def defined_in
    puts 'This method is defined in BasicClass'
    super
  end
end
{% endhighlight %}

There are four new modules: `ModuleIncludedToBasicClass`, `ModulePrependedToBasicClass`,  `ModuleIncludedToSuperClass`, `ModulePrependedToSuperClass`. Their names should reveal their responsibilities clearly. There is also an instance method `defined_in` defined in every module and class. It prints where it is defined and then calls `super` to call a method with the same name defined higher up in the ancestors hierarchy.

There are two ways to mixin module, which are both defined in `Module` in standard Ruby library. `include` is the traditional method, and `prepend` was introduced in Ruby 2.0. Their differences are easier to understand by looking at the ancestors hierarchy.

{% highlight ruby %}
BasicClass.ancestors
=> [ModulePrependedToBasicClass, BasicClass, ModuleIncludedToBasicClass, 
ModulePrependedToSuperClass, SuperClass, ModuleIncludedToSuperClass, 
Object, Kernel, BasicObject]

SuperClass.ancestors
=> [ModulePrependedToSuperClass, SuperClass, ModuleIncludedToSuperClass, 
Object, Kernel, BasicObject]
{% endhighlight %}

As you can see, `prepend` puts the prepended module before the receiving module, whereas `include` puts the included module after the receiving module. This has implications in overriding methods and `super` calls.

{% highlight ruby %}
basic_class_instance = BasicClass.new
=> #<BasicClass:0x007fbf9b2527b0>
basic_class_instance.defined_in
This method is defined in ModulePrependedToBasicClass
This method is defined in BasicClass
This method is defined in ModuleIncludedToBasicClass
This method is defined in ModulePrependedToSuperClass
This method is defined in SuperClass
This method is defined in ModuleIncludedToSuperClass
NoMethodError: super: no superclass method `defined_in' for #<BasicClass:0x007fbf9b2527b0>
{% endhighlight %}

You can see that the order of method call follows the ancestor hierarchy. The `NoMethodError` at the end occurs because there's no `defined_in` method in `Object`.

A valuable insight here is that module mixin is just another way of implementing inheritance and not something magical. It is, however, a very easy to use way to implement multiple inheritance.

{% highlight ruby %}
BasicClass.instance_method(:method_included_to_super_class)
=> #<UnboundMethod: BasicClass(ModuleIncludedToSuperClass)#method_included_to_super_class>

BasicClass.instance_method(:method_prepended_to_basic_class)
=> #<UnboundMethod: BasicClass(ModulePrependedToBasicClass)#method_prepended_to_basic_class>
{% endhighlight %}

Calling `instance_method` tells us where the instance methods available to `BasicClass` are actually defined. There's nothing unexpected here.

## Interlude
This should cover the basics of Ruby method dispatch system. For most of Ruby programming, it would be sufficient to just understand the material so far. 

In the next part, I will discuss metaclass and singleton methods. Those are used much less often, and require a deeper understanding of Ruby.
