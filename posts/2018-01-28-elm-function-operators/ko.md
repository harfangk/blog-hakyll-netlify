---
title: 엘름 함수 연산자(|>, <|, >>, <<)에 대한 간단한 설명
---

## 요약

엘름 언어에 대해서 기본적인 것을 배우고 나서 엘름 코드를 읽다보면 `>>`, `<<`, `|>`, `<|` 같은 기묘한 연산자를 보게 되는데, 이는 함수를 조합하거나 적용할 때 사용되는 함수입니다. 이 글에서는 제가 해당 연산자를 언제 어떻게 쓰는지에 대해서 간단히 이야기해보겠습니다. 하지만 먼저 간단히 요점만 정리하면 다음과 같습니다.

1. `|>`와 `<|`는 데이터의 흐름을 시각적으로 명확히 표현하고 싶을 때 사용합니다. 괄호를 중첩해서 사용해도 똑같은 의미를 나타낼 수 있지만 `|>`나 `<|` 연산자를 쓰면 더 명확합니다. 

2. `>>`와 `<<`는 데이터 흐름과는 무관하게 함수의 조합만을 표현하고 싶을 때 사용합니다. 실제 코드에서는 함수 조합을 통해서 새 함수를 정의하고, 그 함수를 `|>`나 `<|`와 함께 사용합니다.

3. `|>`와 `>>`의 쌍이나 `<|`와 `<<`의 쌍만 사용하고, 둘을 섞어서 사용하지 않습니다. 연산자의 모양이 방향에 대한 의미를 내포하고 있기 때문에 서로 다른 방향을 나타내는 연산자를 섞어 쓸 경우 뇌에서 인지적 자원을 더 소모하게 됩니다.
   
<!--more-->

## 이런 기묘한 연산자가 정말 필요한가요?

반드시 필요한 것은 아닙니다. 이런 연산자가 없어도 똑같은 기능을 다른 방식으로 얼마든지 표현할 수 있기 때문에 엘름 언어는 여전히 잘 작동할 것입니다. 어차피 이 연산자도 편의성 함수일 뿐이니까요. 하지만 이 연산자를 사용하면 데이터의 흐름이나 코드의 구조를 시각적으로 코드 안에서도 표현할 수 있기 때문에 더 이해하기 쉬운 코드를 작성할 수 있습니다. 예시를 살펴볼까요?

## 예시

### `|>` 연산자

임의의 정수의 리스트가 주어졌을 때, 각 정수의 첫 번째 자리수를 리스트로 돌려주는 함수를 정의해봅시다. 먼저 함수 연산자 없이 간단히 구현해봅시다.

```elm
getFirstDigits : List Int -> List Int
getFirstDigits list =
    let
        stringList = List.map toString list
        firstStringDigits = List.map (String.left 1) stringList
        firstResultIntDigits = List.map (String.toInt) firstStringDigits
        firstIntDigits = List.map (Result.withDefault 0) firstResultIntDigits
    in
        firstIntDigits
```

각 중간 단계의 결과를 변수에 할당하고 다음 단계로 넘겨주었습니다. 이는 명령형 언어에서 흔히 볼 수 있는 일반적인 형태입니다. 하지만 파이프 연산자라고 불리는 `|>`를 사용하면 해당 코드를 더 이해하기 쉽게 쓸 수 있습니다.

```elm
-- 여러 줄에 걸쳐 쓸 경우:
getFirstDigits : List Int -> List Int
getFirstDigits list =
    list
    |> List.map toString
    |> List.map (String.left 1)
    |> List.map (String.toInt)
    |> List.map (Result.withDefault 0)

-- 한 줄에 쓸 경우:
getFirstDigits : List Int -> List Int
getFirstDigits list =
    list |> List.map toString |> List.map (String.left 1) |> List.map (String.toInt) |> List.map (Result.withDefault 0)
```

`|>`를 사용하면 중간 변수에 값을 할당할 필요 없이 결과값을 다음 단계로 전달할 수 있습니다. 또한 `|>`는 데이터가 어디로 이동할지를 시각적으로 보여줍니다.

`|>`는 첫 번째 인자를 함수인 두 번째 인자로 전달해줍니다. `(|>) : a -> (a -> b) -> b`라고 정의되어 있죠. 이를 사용하면 이전 단계의 결과값을 다음 단계로 계속 전달해주면서 함수를 여러 개의 독립적인 단계가 아니라 서로 연관된 단계로 구성된 하나의 파이프라인으로 표현할 수 있습니다.

앞서 말했듯이 `|>`는 편의성 함수일 뿐이기 때문에 이를 쓰지 않아도 다음과 같이 표현할 수 있습니다.

```elm
getFirstDigits : List Int -> List Int
getFirstDigits list =
    (List.map (Result.withDefault 0) (List.map (String.toInt) (List.map (String.left 1) (List.map toString list))))
```

하지만 `|>`를 사용하면 괄호를 중첩해서 쓰거나, 다음에 어떤 단계로 이어질 지 직접 기억하고 있을 필요가 없어집니다. 일반적으로 `|>`는 서로 연관된 작업들을 표현하려할 때 유용합니다.

### `<|` 연산자

`<|`는 `|>`를 뒤집은 함수인데, 두 번째 인자를 함수인 첫 번째 인자에 전달하며 `(<|) : (a -> b) -> a -> b`라고 정의되어 있습니다. `<|`를 사용하면 앞서 적은 함수를 다음과 같이 표현할 수 있습니다.

```elm
getFirstDigits : List Int -> List Int
getFirstDigits list =
    List.map (Result.withDefault 0) <| List.map (String.toInt) <| List.map (String.left 1) <| List.map toString <| list
```

예시에서 볼 수 있듯이 `<|`를 사용하면 우측에서 좌측으로 가는 흐름을 시각적으로 표현할 수 있습니다. 비슷하게 우에서 좌로 향하는 수학적 표기법이나 언어에 익숙하다면 매우 편하게 느껴질 것입니다. 개인적으로는 좌상단에서 우하단으로 가는 흐름이 가장 익숙하기 때문에 우에서 좌로 가는 흐름은 불편하게 느껴서 이 연산자는 거의 사용하지 않습니다. 그래도 유용하게 사용할 때가 있습니다. 예를 들어 함수의 마지막 인자가 여러 줄에 걸친 함수일 경우에는 아래와 같이 사용합니다.

```elm
-- <| 사용
test : Test.Test
test "subtraction should have identity property" <|
    \_ ->
        let
            a = negate (2 ^ 50 + 32)
            b = 0
        in
            Expect.equal (a - b) a

-- 괄호 사용
test : Test.Test
test "subtraction should have identity property"
    (\_ ->
        let
            a = negate (2 ^ 50 + 32)
            b = 0
        in
            Expect.equal (a - b) a
    )
```

`<|`를 사용하면 `<|` 다음에 오는 코드를 모두 이해하기 전에는 `<|` 전에 오는 코드를 신경쓸 필요 없다고 시각적으로 명확하게 표현할 수 있어서 유용합니다.

### `>>`, `<<` 연산자

이 두 연산자를 사용하면 두 개의 함수를 하나의 함수로 조합할 수 있습니다. 예시를 볼까요?

```elm
toString : a -> String

String.length : String -> Int

toString >> String.length : a -> Int

String.length << toString : a -> Int
```

함수 시그니처에서 볼 수 있듯이 `>>`와 `<<`는 두 함수를 하나로 조합해줍니다. 두 연산자의 정의에서는 이를 보다 추상적으로 표현하고 있습니다.

```elm
(<<) : (b -> c) -> (a -> b) -> (a -> c)

(>>) : (a -> b) -> (b -> c) -> (a -> c)
```

이 둘은 서로 반대 방향으로 작동한다는 것을 제외하면 기능적으로는 동일합니다. 어떤 방향으로 작동하는지는 연산자의 화살표 방향을 보면 알 수 있습니다. 하지만 거의 동일하다면 둘 중 어떤 것을 써야할까요? 이는 개인적 취향에 달려있다고 봅니다. 조합 연산자를 파이프 연산자와 함께 사용하면 예시 함수를 다음과 같이 표현할 수 있습니다.

```elm
getFirstDigits : List Int -> List Int
getFirstDigits list =
    List.map (Result.withDefault 0) << List.map (String.toInt) << List.map (String.left 1) << List.map toString <| list

getFirstDigits : List Int -> List Int
getFirstDigits list =
    list |> List.map toString >> List.map (String.left 1) >> List.map (String.toInt) >> List.map (Result.withDefault 0)
```

우에서 좌 흐름을 선호한다면 `<<`와 `<|`의 쌍을 사용하시고, 반대로 좌에서 우 흐름을 선호하시면 `>>`와 `|>`의 쌍을 사용하시면 됩니다. 단, 방향이 다른 연산자를 섞어 쓸 경우 뇌에 부담을 주니 섞어 쓰지만 말아주세요.

함수 조합 연산자는 언제 쓸모가 있을까요? 추상적으로 말하자면 이를 사용해서 여러 함수를 조합해서 보다 큰 함수를 만들어낼 수 있습니다. 실질적으로는 함수를 작성할 때 매우 자유롭게 표현할 수 있게됩니다. 예시 함수를 다시 살펴봅시다.

```elm
getFirstDigits : List Int -> List Int
getFirstDigits list =
    list
    |> List.map toString
    |> String.left 1
    |> List.map (String.toInt >> Result.withDefault 0)
```

여기서는 `>>`을 사용해서 두 개의 작은 함수를 묶어내어 문자열에서 정수로 변환하는 함수를 만들었습니다. 그 두 함수는 독립적일 때보다는 하나로 조합되었을 때 보다 의미있다고 생각했기 때문입니다. 이렇게 논리적으로 긴밀히 연결된 함수들을 하나의 함수로 묶어낼 때 `>>`를 가장 유용하게 사용할 수 있습니다. 물론 새 함수로 분리해낼 수도 있지만 `>>`를 사용하면 인라인에서도 같은 일을 할 수도 있습니다.

## 함수 연산자를 사용할 때 주의점

이 글에서 예시로 든 함수를 표현할 수 있는 방법은 정말 많습니다. 예를 들어 아래와 같이 작성할 수도 있습니다.

```elm
-- 별도 함수로 분리
getFirstDigits : List Int -> List Int
getFirstDigits list =
    list
    |> List.map toString
    |> String.left 1
    |> List.map stringToInt

stringToInt : String -> Int
stringToInt s = String.toInt << Result.withDefault <| s
```

저 함수를 작성할 수 있는 방법을 이 글에서만 9가지를 보여주었습니다. 함수 연산자가 없었다면 괄호를 중첩해서 쓰거나, 중간 변수를 할당하거나, 새 함수를 정의하는 등의 방법만 쓸 수 있었을 것입니다. 하지만 함수 연산자를 쓰면 코드를 작성할 때 표현할 수 있는 방식이 훨씬 다채로워집니다.

하지만 이를 남용하면 오히려 더 코드가 혼란스러워질 것은 자명합니다. 함수를 정의할 때 다양한 방식을 섞어 쓰면 다른 사람들이 코드를 읽고 이해하기 더 어려워집니다. 물론 여기에는 미래의 나 자신도 포함되죠. 그러니 함수 연산자를 사용할 때는 통일된 방식으로 사용할 수 있도록 스스로도 절제하고, 명확한 관습을 만들어두는 것이 중요합니다.

## 결론

엘름에 있는 네 개의 함수 연산자 `>>`, `<<`, `|>`, `<|`를 소개하고 예시 함수를 통해서 이를 어떻게 사용할 수 있는 지를 간단히 살펴보았습니다. 해당 연산자에 대한 학문적인 논의는 의도적으로 이 글에서 다루지 않았는데, 이에 대한 이미 좋은 글이 많아서이기도 하고, 연산자를 실제로 사용하는 방법에 대해서는 오히려 글이 너무 없어서이기도 합니다. 이 글이 그 공백을 조금이나마 메꿀 수 있었으면 좋겠습니다.
