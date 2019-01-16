---
title: Elm에서 동시에 여러개의 Msg를 호출하기
---

Elm Architecture에서 `update` 함수는 `Model` 상태를 변경하는데 사용됩니다. `Model`, `Msg`, `update`의 구조에 따라서 `update` 함수를 호출한 뒤에 다른 `Msg`를 인자로 삼아 `update` 함수를 다시 호출해야할 수도 있습니다. `update`를 재귀적으로 호출하는 것은 간단히 할 수 있습니다.

<!--more-->


```elm
type Msg
    = FirstMsg
    | SecondMsg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FirstMsg -> 
            update SecondMsg model

        SecondMsg -> 
            ( model, Cmd.none )
```

하지만 여러 개의 `Msg`를 호출하고 싶을 수도 있습니다. 그럴 때에는 `Task` 를 사용하면 됩니다. 


```elm
type Msg
    = FirstMsg
    | SecondMsg ()
    | ThirdMsg ()

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FirstMsg -> 
            let
                cmd =
                      Cmd.batch
                          [ Task.perform SecondMsg (Task.succeed ())
                          , Task.perform ThirdMsg (Task.succeed ())
                          ]
            in
                ( model, cmd )

        SecondMsg -> 
            ( model, Cmd.none )

        ThirdMsg ->
            ( model, Cmd.none )
```

이 때 `SecondMsg`와 `ThirdMsg` 사이에 호출 순서가 보장이 안 된다는 문제, 그리고 이렇게 호출되는 `Msg`는 인자를 필요로 한다는 문제가 발생합니다. 무엇보다 중요한 것은 재귀적으로 `update` 함수를 호출하는 것이 좋은 패턴이 아니라고 생각합니다. 정말 다른 방법이 없을 때만 이 방식을 사용해야 합니다.
