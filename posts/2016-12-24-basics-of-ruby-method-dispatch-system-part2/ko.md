---
title: 루비 메서드 디스패치 시스템 이해하기 (Part 2)
---

이 글에서는 루비에서 메서드를 호출했을 때, 루비가 어떤 식으로 이를 처리하는지를 살펴보도록 하겠습니다. 루비 언어를 어느 정도 사용할 줄 안다고 가정합니다.

두 개의 글로 나누어서 구성했는데 첫 번째 글에서는 일반적인 루비 프로그램에서 알아야 될 내용을 다루는데, 구체적으로는 조상 목록, 클래스 상속, 모듈 `include` 및 `prepend` 등입니다.

두 번째 글에서는 더 드물게 사용되는 기능을 살펴보는데 싱글턴 메서드, 싱글턴 클래스, 클래스 메서드 등을 다룹니다.

<!--more-->

## 개요
이전 글에서는 인스턴스 메서드가 호출되는 과정을 이야기했는데, 이 글에서는 싱글턴 메서드와 클래스 메서드를 다룹니다.

글을 읽기 전에 싱글턴이라는 용어의 의미를 한 번 생각해 봅시다. 컴퓨터 공학에서 싱글턴이라는 용어는 보통 싱글턴 패턴을 가리키는데, 이는 클래스가 단 하나의 객체만 생성할 수 있도록 제약하는 디자인 패턴을 의미합니다.

하지만 원래 수학에서 말하는 싱글턴이라는 개념은 그보다 더 광범위합니다. 수학에서 싱글턴은 단 하나의 원소만을 가지는 집합을 의미합니다. {0}, {1}, {551231} 등을 말하죠. 싱글턴 패턴도 이런 관점에서 보면 어떤 클래스에서 만들어진 인스턴스의 집합에 단 하나의 원소만 있는 패턴으로 이해할 수 있습니다.

루비에서 싱글턴이라는 용어는 여러 가지 상황에서 쓰입니다. 이를 싱글턴 패턴이라는 개념을 바탕으로 이해하려면 어렵지만, 수학의 싱글턴이라는 개념에서 바라보면 더 이해하기 쉽습니다. 

그러면 시작해볼까요?

## Level 3: 싱글턴 메서드
루비에서는 특정한 객체만이 유일하게 가지고 있고 다른 객체에는 공유되지 않는 메서드를 정의할 수 있습니다. 단 하나의 객체에만 정의되어 있기 때문에 싱글턴 메서드 `singleton method`라고 불립니다. 

```ruby
class BasicClass
  def basic_class_instance_method
  end
end

basic_class_instance_a = BasicClass.new
basic_class_instance_b = BasicClass.new

basic_class_instance_a.define_singleton_method(:singleton_method_of_a) do
end
```

여기서는 `basic_class_instance_method`라는 인스턴스 메서드를 가진 `BasicClass`를 정의하고, 각각 `basic_class_instance_a`와 `basic_class_instance_b`라고 명명한 `BasicClass`의 인스턴스 두 개를 생성했습니다.

그리고 나서 `singleton_method_of_a`를 `basic_class_instance_a`에 정의했습니다. 의도한 대로 정의가 되었는지 확인을 해봅시다.

```ruby
basic_class_instance_a.method(:basic_class_instance_method).owner
=> BasicClass

basic_class_instance_b.method(:basic_class_instance_method).owner
=> BasicClass

basic_class_instance_a.method(:singleton_method_of_a).onwer
=> #<Class:BasicClass>

basic_class_instance_b.method(:singleton_method_of_a).owner
NameError: undefined method `singleton_method_of_a' for class `BasicClass'
```

`basic_class_instance_method`는 두 인스턴스 모두에서 호출이 가능하며, `BasicClass`에 정의되어 있다고 표시됩니다. 하지만 `singleton_method_of_a`는 `BasicClass`가 아니라 `#<Class:BasicClass>`라는 것에 정의되어 있으며, `basic_class_instance_a`에서는 호출할 수 있지만 `basic_class_instance_b`에서는 호출할 수 없습니다. 특정 객체에 싱글턴 메서드를 추가하는데 성공했습니다.

루비에서는 싱글턴 클래스 `singleton class`를 활용해서 싱글턴 메서드를 구현합니다. 이는 아이겐클래스 `eigenclass`나 메타클래스 `metaclass`라고도 불리지만 공식 명칭이 싱글턴 클래스니까 그렇게 부르도록 하겠습니다. 앞서 보았던 `#<Class:BasicClass>`가 싱글턴 클래스입니다.

그러면 싱글턴 클래스를 살펴보도록 합시다.

## Level 4: 싱글턴 클래스
앞서 `basic_class_instance_a`에 `define_singleton_method`를 호출해서 `singleton_method_of_a`를 정의해 보았습니다.

그 메서드는 `basic_class_instance_a`에만 존재하기 때문에 `BasicClass`에 정의할 수는 없습니다. 객체 내부에 정의할 수도 있겠지만 루비에서는 다른 방식을 사용합니다. 루비는 이런 메서드를 `basic_class_instance_a`의 싱글턴 클래스에 정의합니다. 그러면 싱글턴 클래스는 어디에 있고 어떻게 볼 수 있을까요?

객체가 생성되는 과정에 대해서 생각해봅시다. 클래스 내에 정의된 `initialize` 메서드가 호출되면 그 클래스의 새로운 인스턴스가 생성됩니다.

하지만 루비에는 숨겨진 단계가 몇 개 더 있습니다. `initialize`가 호출되면 일단 그 클래스의 서브클래스가 익명으로 생성됩니다. 그리고 그 익명의 서브클래스의 싱글턴 인스턴스가 생성됩니다. 

이 익명의 서브클래스가 바로 싱글턴 클래스입니다. 각 싱글턴 클래스는 단 하나의 인스턴스만 생성하고요. 생성된 인스턴스의 집합에 단 하나의 원소만 존재하기 때문에 싱글턴 클래스라고 불리는 것입니다.

좀 헷갈릴 수도 있으니 그 과정을 다른 형식으로 반복해서 적어보겠습니다.

1. 클래스 내의 `initialize` 메서드가 호출된다.
2. 해당 클래스의 새로운 서브클래스가 익명으로 생성된다. 이런 익명의 서브클래스를 싱글턴 클래스라고 칭한다.
3. 싱글턴 클래스의 유일한 인스턴스가 새로 생성된다.

실제 코드 상에서는 어떻게 나타나는지 살펴봅시다.

### 싱글턴 클래스 확인하기

```ruby
basic_class_instance_a.class
=> BasicClass

basic_class_instance_b.class
=> BasicClass

basic_class_instance_a.singleton_class
=> #<BasicClass:0x007fb5a2002760>

basic_class_instance_b.singleton_class
=> #<BasicClass:0x007fb5a1890720>
```

`class`와 `singleton_class`는  루비 코어 라이브러리의 `Object`에 정의된 메서드로 각각 해당 객체의 클래스와 싱글턴 클래스를 반환합니다.

`#<BasicClass:0x007fb5a2002760>`와 `#<BasicClass:0x007fb5a1890720>`가 루비 인터프리터에서 보는 `basic_class_instance_a`와 `basic_class_instance_b`의 싱글턴 클래스입니다. 보시다시피 `BasicClass`와는 다릅니다.

싱글턴 클래스는 또한 조상 목록에서도 확인할 수 있습니다.

```ruby
basic_class_instance_a.singleton_class.superclass
=> BasicClass

basic_class_instance_a.class.ancestors
=> [BasicClass, Object, Kernel, BasicObject]

basic_class_instance_a.singleton_class.ancestors
=> [#<Class:#<BasicClass:0x007fb5a2002760>>, BasicClass, Object, Kernel, BasicObject]
```

여기서 `#<Class:#<BasicClass:0x007fb5a2002760>>`가 실제로 `BasicClass`인 것을 확인할 수 있습니다.

```ruby
basic_class_instance_a.singleton_class.new
TypeError: can't create instance of singleton class
```

그리고 싱글턴 클래스에서 인스턴스를 만들려 하면 루비에서 에러를 냅니다.

### 싱글턴 메서드는 싱글턴 클래스에 정의된다

`method`를 사용해서 싱글턴 클래스를 살펴볼 수도 있습니다.

```ruby
basic_class_instance_a.method(:basic_class_instance_method)
=> #<Method: BasicClass#basic_class_instance_method>

basic_class_instance_a.method(:singleton_method_of_a)
=> #<Method: #<BasicClass:0x007fb5a2002760>.singleton_method_of_a>

basic_class_instance_a.singleton_methods
=> [:singleton_method_of_a]
```

`basic_class_instance_method`는 `BasicClass`에 정의되어 있고, `singleton_method_of_a`는 `#<BasicClass:0x007fb5a2002760>`에 정의되어 있다고 나옵니다. 표기법이 살짝 다른 것도 확인할 수 있습니다. 싱글턴 클래스에는 `#`가 앞에 붙어있고, 메서드 표시에는 `#` 대신 `.`가 사용됩니다.

`singleton_methods`메서드는 루비 코어 라이브러리의 `Object`에 정의되어 있으며 해당 객체의 싱글턴 메서드를 어레이로 반환합니다.

## Level 5: 클래스 메서드
이제 클래스 메서드를 살펴봅시다. 클래스 메서드를 설명하기 전에 왜 이해하기 더 어려운 싱글턴 메서드와 싱글턴 클래스부터 다루었는지 궁금해하실 수도 있는데, 루비에서 이 두 개념을 사용해서 클래스 메서드를 구현하기 때문에 그랬습니다.

이전 글에서 루비에서는 모든 것이 객체고, 모든 클래스는 루비 코어 라이브러리의 `Class`의 인스턴스라고 언급했습니다. 그리고 이 글에서는 인스턴스가 실제로는 어떤 과정을 통해서 새로 생성되는지를 이야기했습니다. 이 두 가지 사실을 연결해보면 새로운 클래스가 `Class`의 인스턴스로써 생성될 때도 동일한 과정이 적용된다는 사실을 유추할 수 있습니다.

정리해봅시다. 각 클래스에는 고유의 싱글턴 클래스가 있습니다. 해당 클래스의 싱글턴 메서드는 그 클래스의 싱글턴 클래스에 정의됩니다. 그리고 우리가 보통 클래스 메서드라고 부르는 것이 바로 그 싱글턴 메서드입니다.

코드를 통해서 확인해 봅시다. 

```ruby
class BasicClass
  def self.basic_class_class_method_a
  end
  
  def basic_class_instance_method
  end
end

basic_class_instance_a = BasicClass.new
```

`BasicClass`를 새로 정의해 보았습니다. 보시다시피 `basic_class_class_method_a`라는 클래스 메서드를 가지고 있습니다. 메서드를 몇 개 호출해서 상태를 살펴봅시다.

```ruby
BasicClass.class
=> Class

BasicClass.singleton_class
=> #<Class:BasicClass>

BasicClass.singleton_methods
=> [:basic_class_class_method_a]
```

이 결과를 해석하면 다음과 같은 사실을 알 수 있습니다.

* `BasicClass`는 `Class`의 인스턴스다.
* `BasicClass`에는 `#<Class:BasicClass>`라는 싱글턴 클래스가 있다.
* `BasicClass`의 클래스 메서드인 `basic_class_class_method_a`는 `BasicClass`의 싱글턴 메서드이기도 하다.

새로운 싱글턴 메서드를 정의하고, 클래스 메서드를 호출하듯이 호출해봅시다.

```ruby
BasicClass.define_singleton_method(:basic_class_class_method_b) do
  return "I am :basic_class_class_method_b!"
end

BasicClass.basic_class_class_method_b
=> "I am :basic_class_class_method_b!"

BasicClass.singleton_methods
=> [:basic_class_class_method_a, :basic_class_class_method_b]
```

이를 통해서 루비의 클래서 메서드는 클래스의 싱글턴 메서드를 특별히 일컫는 이름이라는 것을 재차 확인할 수 있습니다.

아직 이해하기 조금 어려울 수도 있습니다. 중요한 것은 클래스도 그냥 평범한 객체라고 생각하는 것입니다. 클래스는 다른 객체를 만들수 있다는 특별한 기능을 가지고 있기는 하지만 여전히 평범한 객체일 뿐입니다. 다른 객체와 일반적인 특성을 공유하며, 싱글턴 클래스와 싱글턴 메서드를 가지고 있다는 것은 모든 루비 객체가 일반적으로 가지는 특성 중 일부입니다. 이를 이해한다면 루비에서 클래스 메서드를 구현한 방식이 그렇게 이상하게 느껴지지는 않을 것입니다. 

## Level 6: 클래스 메서드의 메서드 디스패치 시스템
```ruby
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
```

`BasicModule`, `SuperClass`, `BasicClass`를 정의했는데 `BasicClass`는 `SuperClass`로부터 상속받고 `BasicModule`을 `include`합니다. 

`BasicClass`와 싱글턴 클래스의 조상 목록을 살펴봅시다.

```ruby
BasicClass.ancestors
=> [BasicClass, BasicModule, SuperClass, Object, Kernel, BasicObject]

BasicClass.singleton_class.ancestors
=> [#<Class:BasicClass>, #<Class:SuperClass>, #<Class:Object>,
 #<Class:BasicObject>, Class, Module, Object, Kernel, BasicObject]
 
BasicClass.singleton_class.class
=> Class
```

`BasicClass`의 조상 목록은 예상했던 대로입니다. 싱글턴 클래스의 조상 목록은 더 흥미롭습니다. 여기서 다음 사실을 관찰할 수 있습니다.

* 싱글턴 클래스는 자신의 싱글턴 슈퍼클래스로부터 상속받는다.
* 싱글턴 클래스는 루비 코어 라이브러리 `Class`의 인스턴스다.
* 모든 싱글턴 클래스는 `Class`로부터 상속받는다.
* 모듈의 싱글턴 클래스는 조상 목록에 포함되지 않는다. 클래스의 싱글턴 클래스만 서로 상속받는다.

첫 세 가지에서는 슈퍼클래스의 클래스 메서드가 어떻게 서브클래스에 상속되는지를 알 수 있습니다. 

루비 인터프리터가 인스턴스 매서드를 호출할 때 해당 인스턴스의 클래스의 조상 목록을 거슬러 올라가듯이, 클래스 메서드를 호출할 때는 해당 클래스의 싱글턴 클래스의 조상 목록을 거슬러 올라갑니다. 

네 번째 사실은 상당히 중요합니다. 모듈 메서드는 상속되지 않고 그 모듈에서만 직접 호출할 수 있다는 것을 알 수 있습니다.

`Module`과 `Class`를 비교하는 것도 흥미로운 주제이기는 하나 이 글에서 다루는 범위 밖입니다.

## 보너스

### 인스턴스 메서드와 클래스 메서드는 같은 메서드를 다르게 부르는 것 뿐이다

```ruby
# 첫 번째 쌍
BasicClass.instance_methods
=> [:basic_class_instance_method_a, :basic_module_instance_method_a, :super_class_instance_method_a, ...]

basic_class_instance_a.methods
=> [:basic_class_instance_method_a, :basic_module_instance_method_a, :super_class_instance_method_a, ...]

# 두 번째 쌍
BasicClass.singleton_class.instance_methods
=> [:basic_class_class_method_a, :super_class_class_method_a, ...]

BasicClass.methods
=> [:basic_class_class_method_a, :super_class_class_method_a, ...]
```

인스턴스 메서드와 클래스 메서드는 동일한 메서드를 다른 관점에서 본 것입니다.

첫 번째 쌍을 보면 `basic_class_instance_a`는 `BasicClass`의 인스턴스이며, `BasicClass`에 정의된 인스턴스 메서드를 가지고 있습니다.

두 번째 쌍을 살펴봅시다. `BasicClass`는 `BasicClass.singleton_class`의 인스턴스이며, `BasicClass.singleton_class`에 정의된 인스턴스 메서드를 가지고 있습니다.

`BasicClass`라는 객체가 클래스이기 때문에 이 객체가 가진 메서드를 특별히 클래스 메서드라고 부릅니다. 즉 `BasicClass`의 클래스 메서드는 `BasicClass.singleton_class`의 인스턴스 메서드와 동일합니다. 

이런 개념을 직접적으로 사용할 일은 별로 없겠지만 객체지향프로그램의 재귀적 구조를 볼 수 있어 흥미롭습니다.

### 싱글턴 클래스에 접근하는 다른 방법
일반적으로 싱글턴 클래스를 다루는 방법은 세 가지가 있습니다.

```ruby
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
```

첫 번째 방식은 루비 코어 `Object`에 정의된 `define_singleton_method`를 호출합니다.

두 번째 방식은 `basic_class_instance_a`의 싱글턴 클래스에 직접 접근해 열어서 메서드를 정의합니다.

세 번째 방법은 루비 코어 `Object`에 정의된 `extend`를 호출합니다. 이 메서드는 파라미터에 주어진 모듈의 모든 인스턴스 메서드를 `extend`가 호출된 객체에 싱글턴 메서드로 추가해줍니다.

## 마무리

이 긴 글을 읽어주셔서 감사합니다. 2부에서는 싱글턴 메서드, 싱글턴 클래스, 클래스 메서드, 싱글턴 클래스의 조상 목록, 그리고 클래스 메서드의 메서드 디스패치 시스템을 간략히 살펴보았습니다. 

메타프로그래밍을 할 것이 아니라면 이 글에서 다룬 내용을 깊이 있기 이해할 필요는 없지만 이는 자체적으로도 흥미로운 내용입니다. 이 글을 통해서 루비의 내부 작동 방식을 조금 더 잘 이해하실 수 있었으면 합니다. 
