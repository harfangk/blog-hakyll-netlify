---
title: Basics of Ruby Method Dispatch System (Part 2)
---

In this post, I will explain the basics of how a method call works in Ruby. I'll assume that readers have some familiarity with Ruby language. 

The post is in two parts. The first part covers what you need to know about typical Ruby program. It covers: ancestors hierarchy, class inheritance, and module `include` and `prepend`. 

The second part digs into tools that are used less often. It covers: singleton method, singleton class, and class methods.

<!--more-->

## Overview
My previous post discussed how instance methods get called. Here I will talk about singleton methods and class methods. 

It would be helpful to think about the meaning of singleton before going through this post. In computer science, the term singleton is usually used to mean singleton pattern, which describes a design pattern that restricts a class to instantiate only one object. 

But the original concept of singleton in mathematics is broader than that. In mathematics, a singleton is a set with exactly one element, i.e. {0}, {1}, {551231}. In the same vein, singleton pattern means that the set of instances of a class has exactly one element.

In Ruby, the term singleton is used in a few different contexts. Those usages might be confusing when thought in terms of singleton pattern, but they are easier to comprehend when you think in terms of mathematical singleton.

Let's get started then.

## Level 3: Singleton method
In Ruby, it is possible to define methods that are unique to a particular object and not shared by other objects. They are called singleton methods because they are defined to exactly one object. 

{% highlight ruby %}
class BasicClass
  def basic_class_instance_method
  end
end

basic_class_instance_a = BasicClass.new
basic_class_instance_b = BasicClass.new

basic_class_instance_a.define_singleton_method(:singleton_method_of_a) do
end
{% endhighlight %}

Here we defined a `BasicClass` that has `basic_class_instance_method` as its instance method, and created two instances of `BasicClass` each called `basic_class_instance_a` and `basic_class_instance_b`.

Then we defined `singleton_method_of_a` to `basic_class_instance_a`. Let's see if those methods are defined as we intended.

{% highlight ruby %}
basic_class_instance_a.method(:basic_class_instance_method).owner
=> BasicClass

basic_class_instance_b.method(:basic_class_instance_method).owner
=> BasicClass

basic_class_instance_a.method(:singleton_method_of_a).onwer
=> #<Class:BasicClass>

basic_class_instance_b.method(:singleton_method_of_a).owner
NameError: undefined method `singleton_method_of_a' for class `BasicClass'
{% endhighlight %}

`basic_class_instance_method` is defined in `BasicClass` and can be called from both instances. `singleton_method_of_a`, however, is defined in something called `#<Class:BasicClass>` and can be called only from `basic_class_instance_a` and not from `basic_class_instance_b`. We successfully created a singleton method to an object.

Ruby enables this behavior through an implementation called `singleton class`. It is also called `eigenclass` and `metaclass`, but the official term is `singleton class` so I will stick to it. And yes, `#<Class:BasicClass>` is a singleton class.

Let's dive into singleton class.

## Level 4: Singleton class
In the previous section, we called `define_singleton_method` on `basic_class_instance_a` and defined `singleton_method_of_a`.

Since the method is unique to `basic_class_instance_a`, it cannot be defined in `BasicClass`. It could be defined in the object itself, but that's not how Ruby does it. The method is actually defined in the singleton class of `basic_class_instance_a`. But then what is a singleton class and how can we see it?

Let's think about how an object is instantiated. The `initialize` method defined in a class is called, and a new object of that class is instantiated, right?

In Ruby, however, there are some steps hidden behind that process. When `initialize` is called, an anonymous subclass of the class is created first, then the object is instantiated as a singleton instance of that anonymous subclass. 

This anonymous subclass is called a singleton class. Each singleton class instantiates only one object. That is why it is called a singleton class - there's only one element in the set of its instances.

This is a confusing concept, so let me repeat it in a different way.

1. `initialize` method in a class is called.
2. An anonymous subclass of that class is created. Such anonymous subclasses are called singleton classes.
3. The new and only instance of the singleton class is created.

Let's try to see how they are represented in actual code.

### Inspecting singleton class

{% highlight ruby %}
basic_class_instance_a.class
=> BasicClass

basic_class_instance_b.class
=> BasicClass

basic_class_instance_a.singleton_class
=> #<BasicClass:0x007fb5a2002760>

basic_class_instance_b.singleton_class
=> #<BasicClass:0x007fb5a1890720>
{% endhighlight %}

`class` and `singleton_class` methods are defined in `Object` of Ruby core library, and respectively return the class and singleton class of the object.

`#<BasicClass:0x007fb5a2002760>` and `#<BasicClass:0x007fb5a1890720>` are Ruby interpreter's references to the singleton classes of `basic_class_instance_a` and `basic_class_instance_b`. As you can see, they are different from `BasicClass`.

The singleton class can also be observed in the ancestors hierarchy.

{% highlight ruby %}
basic_class_instance_a.singleton_class.superclass
=> BasicClass

basic_class_instance_a.class.ancestors
=> [BasicClass, Object, Kernel, BasicObject]

basic_class_instance_a.singleton_class.ancestors
=> [#<Class:#<BasicClass:0x007fb5a2002760>>, BasicClass, Object, Kernel, BasicObject]
{% endhighlight %}

Here you can see that `#<Class:#<BasicClass:0x007fb5a2002760>>` is actually a subclass of `BasicClass`.

{% highlight ruby %}
basic_class_instance_a.singleton_class.new
TypeError: can't create instance of singleton class
{% endhighlight %}

When you try to instantiate an object from a singleton class, Ruby will give you an error.

### Singleton methods are defined in singleton classes

Using `method` is another way to look at the singleton classes.

{% highlight ruby %}
basic_class_instance_a.method(:basic_class_instance_method)
=> #<Method: BasicClass#basic_class_instance_method>

basic_class_instance_a.method(:singleton_method_of_a)
=> #<Method: #<BasicClass:0x007fb5a2002760>.singleton_method_of_a>

basic_class_instance_a.singleton_methods
=> [:singleton_method_of_a]
{% endhighlight %}

It tells that `basic_class_instance_method` is defined within `BasicClass`, but `singleton_method_of_a` is defined in `#<BasicClass:0x007fb5a2002760>`. Note that the notation is slightly different, too. For singleton class, `#` is prepended, and `.` is used instead of `#` for the method. 

`singleton_methods` method is defined in `Object` and returns the array of singleton methods of that object.

## Level 5: Class method
Now we turn to class method. You might be wondering why I would talk about class method after talking about more obscure topics like singleton method and singleton class. It's because class method in Ruby is actually implemented using those two concepts. 

In the previous post, I mentioned that everything in Ruby is an object, and that a class is an instance of `Class` from Ruby core library. Then in the previous section, we discussed what actually happens behind the scene when an object is instantiated. The same process applies to when a new class is instantiated from `Class`. 

In other words, each class has its own singleton class. Singleton methods of the class are defined in its singleton class. Those singleton methods are what we call class methods.

Let's look at the code to verify this.

{% highlight ruby %}
class BasicClass
  def basic_class_class_method_a
  end
  
  def basic_class_instance_method
  end
end

basic_class_instance_a = BasicClass.new
{% endhighlight %}

This is a new definition of `BasicClass`. As you can see, it now has a class method `basic_class_class_method_a`. Let's call a few methods on it.

{% highlight ruby %}
BasicClass.class
=> Class

BasicClass.singleton_class
=> #<Class:BasicClass>

BasicClass.singleton_methods
=> [:basic_class_class_method_a]
{% endhighlight %}

The results mean that:

* `BasicClass` is an instance of `Class`.
* `BasicClass` has a singleton class called `#<Class:BasicClass>`.
* `basic_class_class_method_a`, a class method of `BasicClass`, is listed as a singleton method of `BasicClass`.

Let's define a new singleton method and call it like we would call class methods.

{% highlight ruby %}
BasicClass.define_singleton_method(:basic_class_class_method_b) do
  return "I am :basic_class_class_method_b!"
end

BasicClass.basic_class_class_method_b
=> "I am :basic_class_class_method_b!"

BasicClass.singleton_methods
=> [:basic_class_class_method_a, :basic_class_class_method_b]
{% endhighlight %}

Here we see once again that Ruby class methods are actually special names we give to singleton methods of classes.

You might still have hard time understanding this. The key is to understand that class is just another object. It does have a special capability to function as a template to construct other objects, but it is still an object. It shares characteristics common to other objects, and having singleton class and singleton method are a few of those shared characteristics. Once you wrap your head around that concept, this implementation detail of Ruby would not feel so alien.

## Level 6: Method dispatch of class methods
{% highlight ruby %}
module BasicModule 
  def self.basic_module_module_method_a
  end

  def basic_module_instance_method_a
  end
end

class SuperClass
  def self.super_class_class_method_a
  end

  def super_class_instance_method_a
  end
end

class BasicClass < SuperClass
  include BasicModule
  def self.basic_class_class_method_a
  end

  def basic_class_instance_method_a
  end
end

basic_class_instance_a = BasicClass.new
{% endhighlight %}

Here we have `BasicModule`, `SuperClass`, and `BasicClass`. `BasicClass` inherits from `SuperClass` and includes `BasicModule`. 

Let's check the ancestors hierarchy of `BasicClass` and its singleton class.

{% highlight ruby %}
BasicClass.ancestors
=> [BasicClass, BasicModule, SuperClass, Object, Kernel, BasicObject]

BasicClass.singleton_class.ancestors
=> [#<Class:BasicClass>, #<Class:SuperClass>, #<Class:Object>,
 #<Class:BasicObject>, Class, Module, Object, Kernel, BasicObject]
 
BasicClass.singleton_class.class
=> Class
{% endhighlight %}

The ancestors hierarchy of `BasicClass` is as expected. Its singleton class has more interesting ancestors. 

From here, we can observe the following:

* Singleton classes inherit from their singleton superclasses.
* Singleton classes are instances of `Class` in Ruby core.
* All singleton classes inherit from `Class`.
* Singleton classes of modules are not part of the ancestors hiarchy. Only the singleton classes of class are inherited.

First three observations are interesting to know. They explain how class methods of superclass are inherited to subclasses. 

Just like Ruby interpreter iterates through ancestors hierarchy of class to dispatch instance methods, it iterates through ancestors hierarchy of singleton class to dispatch class methods.

Last observation is quite important for actual Ruby programming. It tells that module methods are not inherited and can only be called directly from the module itself. 

Comparing `Module` and `Class` is another interesting topic, but it's beyond the scope of this article.

## Bonus material

### Instance methods and class methods are two sides of the same coin

{% highlight ruby %}
# First pair
BasicClass.instance_methods
=> [:basic_class_instance_method_a, :basic_module_instance_method_a, :super_class_instance_method_a, ...]

basic_class_instance_a.methods
=> [:basic_class_instance_method_a, :basic_module_instance_method_a, :super_class_instance_method_a, ...]

# Second pair
BasicClass.singleton_class.instance_methods
=> [:basic_class_class_method_a, :super_class_class_method_a, ...]

BasicClass.methods
=> [:basic_class_class_method_a, :super_class_class_method_a, ...]
{% endhighlight %}

Instance methods and class methods refer to same methods viewed from different perspectives. 

Look at the first pair. `basic_class_instance_a` is an instance of `BasicClass`, so it has instance methods defined in `BasicClass`.

Let's look at the next pair then. `BasicClass` is an instance of `BasicClass.singleton_class`, so it has instance methods defined in `BasicClass.singleton_class`.

Since `BasicClass` is a class, we give its methods a special name: class methods. So class methods of `BasicClass` are instance methods of its singleton class. 

This concept has little direct use in code, but is a thought-provoking example about recursive nature of object-oriented programming. 

### Other ways to access singleton class
There are three commonly used ways to access and modify singleton classes. Let's go over what they are.

{% highlight ruby %}
class BasicClass
end

basic_class_instance_a = BasicClass.new

basic_class_instance_a.singleton_methods
=> []

basic_class_instance_a.define_singleton_method(:singleton_method_a) do
end

basic_class_instance_a.singleton_methods
=> [:singleton_method_a]

class << basic_class_instance_a
  def singleton_method_b
  end
end

basic_class_instance_a.singleton_methods
=> [:singleton_method_a, :singleton_method_b]

module BasicModule 
  def singleton_method_c
  end
end

basic_class_instance_a.extend(BasicModule)

basic_class_instance_a.singleton_methods
=> [:singleton_method_a, :singleton_method_b, :singleton_method_c]
{% endhighlight %}

First one calls `define_singleton_method` from `Object` in Ruby core.

Second one directly accesses and opens the singleton class of `basic_class_instance_a` to define the method.

Third one calls `extend` from `Object` in Ruby core, which adds all instance methods of the modules given as parameter.

## Epilogue

Thank you for patiently reading through this lengthy article. In part 2, we talked about singleton method, singleton class, class method, ancestors hierarchy of singleton classes, and the method dispatching of class methods. 

You don't really need to directly use the concepts we've explored here unless you want to do metaprogramming, but it's still an interesting subject. I hope it helped you understand the inner workings of Ruby more.
