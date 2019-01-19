---
title: 루비 메서드 디스패치 시스템 이해하기 (Part 1)
---

이 글에서는 루비에서 메서드를 호출했을 때, 루비가 어떤 식으로 이를 처리하는지를 살펴보도록 하겠습니다. 루비 언어를 어느 정도 사용할 줄 안다고 가정합니다.

두 개의 글로 나누어서 구성했는데 첫 번째 글에서는 일반적인 루비 프로그램에서 알아야 될 내용을 다루는데, 구체적으로는 조상 목록, 클래스 상속, 모듈 `include` 및 `prepend` 등입니다.

두 번째 글에서는 더 드물게 사용되는 기능을 살펴보는데 싱글턴 메서드, 싱글턴 클래스, 클래스 메서드 등을 다룹니다.

<!--more-->

## 개요
메서드 디스패치 시스템에서 가장 핵심적인 부분은 객체의 조상입니다. 여기서 객체의 조상이란 해당 객체가 상속을 받는 모든 클래스와 모듈을 말합니다.

어떤 객체에 메서드가 호출되면 루비 인터프리터는 메서드의 정의를 찾기 위해 그 객체의 클래스를 살펴봅니다. 거기서 메서드의 정의를 찾지 못하면 객체의 조상 목록을 거쳐 올라가며 메서드의 정의를 찾아봅니다. 만약 마지막 조상까지 살펴봤는데도 메서드의 정의를 찾지 못 한다면 다음 단계로 넘어갑니다.

루비 인터프리터는 이번에는 `method_missing` 메서드가 정의되어 있나 살펴보면서 조상 목록을 올라갑니다. 조상 중에 `method_missing`이 정의된 것이 없다면 `NoMethodError`를 반환합니다.

목록으로 정리해볼까요?

1. 메서드의 정의를 찾기 위해서 해당 객체의 클래스를 살펴본다.
2. 조상 목록을 거슬러 올라가면서 메서드의 정의를 계속 찾아본다.
3. `method_missing`이 정의되어 있는지 해당 객체의 클래스를 살펴본다.
4. `method_missing`의 정의를 찾기 위해 조상 목록을 거슬러 올라간다.
5. `NoMethodError`를 반환한다.

`method_missing`은 메타프로그래밍에 매우 유용하게 사용되는 도구로, 정의되지 않은 메서드를 프로그래머가 런타임에 다룰 수 있도록 해줍니다. 하지만 이 글의 주제를 벗어나므로 여기서는 다시 언급하지 않겠습니다. 

## Level 0: 단일 클래스

먼저 루비 프로그램에서 일반적으로 사용되는 구조 중에서 가장 간단한 것을 살펴보겠습니다. 이 글에서 사용할 기본 개념과 도구를 여기서 다룰 것이기 때문에 아마 가장 긴 부분이 될 것입니다. 

```ruby
class BasicClass
  def basic_class_method
  end
end

basic_class_instance = BasicClass.new
=> #<BasicClass:0x007fedd2261138>
```

`basic_class_method`라는 인스턴스 메서드를 가진 `BasicClass`를 정의했고, `BasicClass`의 새 인스턴스를 생성해서 `basic_class_instance`라고 이름 붙였습니다.

```ruby
basic_class_instance.class
=> BasicClass

BasicClass.class
=> Class
```

`class` 메서드는 루비 스탠다드 라이브러리의 `Class`에 정의되어 있는 메서드인데, 어떤 객체에 이를 호출하면 그 객체의 클래스를 반환합니다. 코드에 나와 있듯이 `basic_class_instance`는 `BasicClass`의 인스턴스입니다. 생각대로입니다.

하지만 흥미롭게도 `BasicClass`가 `Class`의 인스턴스라는 것도 볼 수 있습니다. 루비에서 프로그래머가 정의하는 모든 클래스는 `Class` 클래스의 인스턴스입니다.

루비에서는 모든 것이 객체입니다. 이는 여러 가지 의미를 가질 수 있는데, 여기서는 객체가 생성되는 부분에 대해서만 생각해 봅시다. 대부분의 객체지향 언어에서 객체는 클래스의 인스턴스로서 생성됩니다. 하지만 루비에서는 모든 것이 객체라고 앞서 말했으며, 클래스도 예외가 아닙니다. 즉, 프로그래머가 정의한 클래스는 사실 `Class` 클래스의 인스턴스로서 생성되는 것이죠. 재귀적인 방식이니 처음 접하는 개념이라면 한 번 시간을 들여 생각해보면 좋습니다.

```ruby
BasicClass.ancestors
=> [BasicClass, Object, Kernel, BasicObject]

BasicClass.class.ancestors
=> [Class, Module, Object, Kernel, BasicObject]

BasicClass.instance_method(:basic_class_method)
=> #<UnboundMethod: BasicClass#basic_class_method>
```

`ancestors` 메서드는 루비 스탠다드 라이브러리에 있는 `Module`에 정의되어 있습니다. 어떤 클래스나 모듈의 조상을 전부 보여줍니다.

`BasicClass`의 조상은 `Object`, `Kernel`, `BasicObject` 셋입니다. 모든 루비 객체는 이 셋으로부터 상속받습니다.

`BasicClass`의 클래스인 `Class`의 조상은 `Module`, `Object`, `Kernel`, `BasicObject` 넷입니다. 루비에서 `Class`는 `Module`의 서브클래스입니다. 조금 이상하게 느껴질 수도 있지만 루비에는 그렇게 구현되어 있습니다.

`Class`에는 `Module`에 없는 기능이 몇 가지 있는데, 예를 들어 객체를 생성하는 기능 등입니다. `Class`와 `Module`의 차이점에 대해서는 별개의 글을 써야 할 것 같으니 여기서는 더 이상 다루지 않겠습니다. 이 글에서 사용할 다음 도구를 볼까요.

```ruby
BasicClass.instance_method(:basic_class_method)
=> #<UnboundMethod: BasicClass#basic_class_method>
```

`instance_method`는 `Module`에 정의되어 있는데, 아규먼트로 받은 인스턴스 메서드를 표현하는 `UnboundMethod`를 반환합니다. 이 경우에는 `basic_class_method`가 `BasicClass`에 `basic_class_method`라는 이름으로 정의되어 있는 `UnboundMethod`라는 것을 의미합니다. 이름에 대한 부분은 중복되는 것처럼 느껴질 수도 있지만 alias를 사용한 메서드를 다룰 때는 매우 유용합니다.

이 글에서는 `instance_method`를 사용해서 인스턴스 메서드가 조상 목록 중 어디에 정의되어 있는지만 살펴볼 것이므로 `UnboundMethod`가 무엇인지에 대해서는 다루지 않겠습니다.

## Level 1: 슈퍼클래스와 서브클래스

이 부분에서는 일반적으로 사용하는 또다른 구조를 살펴보겠습니다. 바로 슈퍼클래스와 서브클래스입니다. 전 부분에서 소개한 도구를 사용해서 살펴보기만 할 것이며 새로운 개념은 없습니다. 

```ruby
class SuperClass
  def super_class_method
  end
end

class BasicClass < SuperClass
  def basic_class_method
  end
end
```

`SuperClass`를 새로 만들고 `BasicClass`가 그로부터 상속 받도록 했습니다. 조상 목록을 살펴봅시다.

```ruby
BasicClass.ancestors
=> [BasicClass, SuperClass, Object, Kernel, BasicObject]

SuperClass.ancestors
=> [SuperClass, Object, Kernel, BasicObject]
```

`BasicClass`에는 `SuperClass`라는 조상이 추가됐습니다. 특별할 것 없는 내용입니다.

`instance_method`를 살펴봅시다.

```ruby
SuperClass.instance_method(:super_class_method)
=> #<UnboundMethod: SuperClass#super_class_method>

BasicClass.instance_method(:basic_class_method)
=> #<UnboundMethod: BasicClass#basic_class_method>

SuperClass.instance_method(:basic_class_method)
NameError: undefined method `basic_class_method' for class `SuperClass'

BasicClass.instance_method(:super_class_method)
=> #<UnboundMethod: BasicClass(SuperClass)#super_class_method>
```

세 번째까지는 별다른 내용이 없습니다.. `SuperClass`에는 `super_class_method`가 있고, `BasicClass`에는 `basic_class_method`가 있고, `SuperClass`에는 `basic_class_method`가 없습니다. 

마지막으로 호출한 결과 값에는 유용한 정보가 있습니다. `BasicClass`가 `super_class_method`를 인스턴스 메서드로 가지고 있지만, 메서드 자체는 `SuperClass`에 정의되어 있다는 것을 알려줍니다. 

메서드 디스패치 시스템 관점에서 이야기하자면 이는 루비 인터프리터가 조상 목록을 한 단계 올라가서 메서드의 정의를 찾았다는 것을 의미합니다.

## Level 2: 모듈 Include와 Prepend

루비에서 모듈 믹싱은 클래스 상속과 더불어 데이터와 함수를 관리하고 네임스페이스를 만들기 위해 사용됩니다. 모듈이 믹싱되면 그 모듈의 상수, 메서드, 모듈 변수가 대상 모듈에 추가됩니다. 조상 목록에서 모듈 믹싱이 어떻게 표현되는지 살펴봅시다.

```ruby
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
```

`ModuleIncludedToBasicClass`, `ModulePrependedToBasicClass`, `ModuleIncludedToSuperClass`, `ModulePrependedToSuperClass`라는 네 개의 모듈이 추가 되었는데, 각자 하는 일은 자명할 것입니다. 또 각 모듈과 클래스마다 `defined_in`이라는 인스턴스 메서드를 새로 정의했습니다. 호출이 되면 자신이 정의된 장소를 출력하고, `super`를 호출해 조상 목록에서 자신과 같은 이름을 가진 메서드를 호출합니다.

모듈을 믹싱하는 방법에는 두 가지가 있는데, 루비 스탠다드 라이브러리의 `Module`에 정의되어 있습니다. `include`는 예전부터 있었고, `prepend`는 루비 2.0에 추가되었습니다. 어떻게 다른지는 조상 목록을 보면 더 잘 이해가 될 것입니다.

```ruby
BasicClass.ancestors
=> [ModulePrependedToBasicClass, BasicClass, ModuleIncludedToBasicClass, 
ModulePrependedToSuperClass, SuperClass, ModuleIncludedToSuperClass, 
Object, Kernel, BasicObject]

SuperClass.ancestors
=> [ModulePrependedToSuperClass, SuperClass, ModuleIncludedToSuperClass, 
Object, Kernel, BasicObject]
```

실행 결과에서 보이듯이 `prepend`는 조상 목록에서 대상 모듈 이전에 호출된 모듈을 추가하는 반면 `include`는 대상 모듈 이후에 호출된 모듈을 추가합니다. 이는 메서드 오버라이드와 `super` 메서드 호출에 영향을 줍니다.

```ruby
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
```

메서드 호출 순서가 조상 목록과 동일한 것을 볼 수 있습니다. 마지막의 `NoMethodError`는 `Object`에 `defined_in`가 정의되어 있지 않아서 발생합니다.

여기서 알 수 있는 점은 모듈 믹싱이 뭔가 특별한 것이 아니고 상속을 구현하는 방법 중 하나일 뿐이라는 것입니다. 그래도 매우 가볍고 다루기 쉽게 다중 상속을 구현하는 방식으로 매우 가치가 높습니다.

```ruby
BasicClass.instance_method(:method_included_to_super_class)
=> #<UnboundMethod: BasicClass(ModuleIncludedToSuperClass)#method_included_to_super_class>

BasicClass.instance_method(:method_prepended_to_basic_class)
=> #<UnboundMethod: BasicClass(ModulePrependedToBasicClass)#method_prepended_to_basic_class>
```

`instance_method`를 호출하면 `BasicClass`에 있는 인스턴스 메서드가 실제로 정의가 된 곳은 어디인지 알 수 있습니다. 예상 외의 결과는 없습니다.

## 막간

루비 메서드 디스패치 시스템의 기본은 이 정도입니다. 일반적인 루비 프로그래밍에는 이 정도만 이해해도 큰 문제가 없습니다.

다음 글에서는 메타클래스와 싱글턴 메서드를 다룰 예정입니다. 이는 상대적으로 드물게 사용되며 루비에 대해서도 더 잘 알아야 사용할 수 있습니다.
