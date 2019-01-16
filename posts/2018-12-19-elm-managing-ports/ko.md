---
title: 엘름에서 포트 함수 관리하기
---

엘름에서 포트 함수 관련 코드를 어떻게 하면 깔끔하게 관리할 수 있을지 고민하다가 두 가지 방식을 정리해보았습니다. 독자가 엘름 아키텍처와 포트 함수의 동작 방식 등은 이미 알고 있다고 가정하겠습니다. 깃헙 리포에 예시 앱을 올려두었으니 아래와 같이 받아서 실행해보실 수 있습니다.

<!--more-->

```bash
git clone harfangk/elm-port-examples
cd elm-port-example
npm install --global elm elm-live
elm-live src/Main.elm --open -- --output=elm.js
```

# 개별 포트

이 방식에서는 엘름과 자바스크립트간 통신마다 하나의 포트 함수를 정의합니다. 포트는 항상 단방향 통신이기 때문에 데이터가 왕복할 경우 한 쌍이 필요합니다. 간단한 예시 코드인데, 전체 코드는 `individual-ports` 브랜치에서 확인해보실 수 있습니다.

```elm
# Main.elm

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RemoveLocalStorageItem ->
            ( model, removeLocalStorageItem model.keyForRemoveItem )

        _ -> ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ gotLocalStorageItem GotLocalStorageItem
        , gotCookies GotCookies
        ]

port setLocalStorageItem : { key : String, value : String } -> Cmd msg
port getLocalStorageItem : String -> Cmd msg
port gotLocalStorageItem : ({ key : String, value : Maybe String } -> msg) -> Sub msg
port removeLocalStorageItem : String -> Cmd msg
port clearLocalStorage : () -> Cmd msg
port getCookies : () -> Cmd msg
port gotCookies : (String -> msg) -> Sub msg
port setCookie : { key : String, value : String } -> Cmd msg
```

개별 포트 방식에서는 단 하나의 기능만 수행하는 구체적인 함수를 정의합니다. 덕분에 입력값과 결과값의 타입을 세밀하게 표기할 수 있어서 컴파일 타임에 타입 안전성이 어느 정도 보장됩니다. 또한 모두 별개의 함수이기 때문에 유의미한 이름을 지어서 함수의 역할을 쉽게 이해할 수 있도록 할 수도 있습니다.

그리고 각 포트 함수가 하나의 작은 기능만 수행하기 때문에 모듈 경계선을 넘어서 영향을 미칠 가능성이 매우 낮습니다. `complex-model-individual-ports` 브랜치에는 조금 더 복잡한 모델 구조에 맞추어 개별 포트 방식으로 예시를 구현해두었는데, `Main` 모듈을 확인해보시면 `Page.Storage.initModel`, `Page.Storage.update`, `Page.Storage.view`, 그리고 `Page.Storage.subscriptions` 네 함수만으로 `Main` 모듈과 `Page.Storage`을 연동하는 것을 볼 수 있습니다. 이렇게 모듈화를 하기 쉽기 때문에 이후에 코드를 교체하거나 수정하는 것도 쉽습니다.

반면에 이 방식을 사용하면 필요한 포트 함수의 갯수가 순식간에 증가합니다. 예시에서는 로컬스토리지와 쿠키 관련 기초적인 기능만 구현했는데도 포트 함수가 10개나 필요했습니다. 더 큰 앱에서는 포트 함수 수가 100개를 넘어가는 것도 순식간이겠죠.

또한 이 방식을 사용하면 모듈화는 쉽지만 각 모듈 내에서 엘름 코드와 엘름-자바스크립트 상호작용 코드가 뒤섞이게 됩니다. `Page.Storage` 모듈을 열어봤을 때 각 `Msg`가 엘름 이벤트를 처리하는지 자바스크립트 이벤트를 처리하는지 한 눈에 알아보기 어렵습니다. 주석을 통해서 어느 정도 보완할 수는 있지만 계속 `Msg`를 추가할수록 결국 관리가 어려워지고요.

마지막으로 각 포트 함수가 이벤트 리스너를 사용하기 때문에 성능에 영향을 미칠 수도 있습니다. 하지만 저도 엘름 내에서 이를 어떤 식으로 관리하고 최적화하는지는 잘 모르기 때문에 불필요한 염려일지도 모르니 이를 감안해주시기 바랍니다.

## 중앙 포트

이 방식에서는 엘름에서 자바스크립트로 가는 통신을 모두 담당할 포트 함수 하나, 그리고 자바스크립트에서 엘름으로 오는 통신을 모두 담당할 포트 함수 하나를 정의합니다. 전체 코드는 `centralized-ports` 브랜치에서 확인할 수 있습니다.

```elm
# Main.elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RemoveLocalStorageItem ->
            let
                portMsg =
                    Port.RemoveLocalStorageItem { key = model.keyForRemoveItem }
                        |> Port.encode
            in
            ( model, Port.sendPortMsg portMsg )

        _ -> ( model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
    Port.gotPortMsg (JD.decodeValue Port.decoder >> GotPortMsg)
    
# Port.elm 

port sendPortMsg : JE.Value -> Cmd msg
port gotPortMsg : (JE.Value -> msg) -> Sub msg

-- Outbound Ports

type OutboundMethod
    = SetLocalStorageItem { key : String, value : String }
    | GetLocalStorageItem { key : String }
    | ClearLocalStorage
    | RemoveLocalStorageItem { key : String }
    | GetCookies
    | SetCookie { key : String, value : String }

encode : OutboundMethod -> JE.Value
encode method =
    case method of
        SetLocalStorageItem { key, value } ->
            let
                payload =
                    JE.object
                        [ ( "key", JE.string key )
                        , ( "value", JE.string value )
                        ]
            in
            JE.object
                [ ( "method", JE.string "setLocalStorageItem" )
                , ( "payload", payload )
                ]
        ...

-- Inbound Ports

type InboundMethod
    = GotLocalStorageItem { key : String, value : String }
    | GotCookies String

decoder : Decoder InboundMethod
decoder =
    JD.field "method" JD.string
        |> JD.andThen decoder_


decoder_ : String -> JD.Decoder InboundMethod
decoder_ method =
    case method of
        "gotLocalStorageItem" ->
            JD.map GotLocalStorageItem (JD.field "payload" kvDecoder)

        "gotCookies" ->
            JD.map GotCookies (JD.at [ "payload", "cookies" ] JD.string)

        _ ->
            JD.fail "Got unregistered inbound port method"
```

중앙 포트 방식은 엘름과 자바스크립트 사이에서 라우터 기능을 수행하는 두 개의 포트 함수를 사용합니다. 이 한 쌍의 함수에서 엘름-자바스크립트 통신을 모두 처리하기 때문에 관련 코드를 하나의 모듈에 집적하여 엘름 코드와 엘름-자바스크립트 통신 코드를 분리하기 쉽고, 따라서 전체 앱 내에서 해당 기능을 이해하고 변경하는 것도 쉬워집니다. 매번 포트 함수를 사용하기 위해 데이터를 인코딩 디코딩할 필요 없이 `Port` 모듈에 정의된 함수를 호출하기만 해도 자바스크립트와 통신할 수 있고, 관련 기능을 변경할 경우 `Port` 모듈에 있는 함수만 변경하면 대부분의 작업이 끝나게 됩니다.

하지만 이 방식을 사용하면 모듈화가 조금 어려집니다. 각 포트 섭스크립션을 처리할 `Msg`를 최상위 `Main` 모듈에서 모두 지정해줘야하기 때문에 최상위 모듈에서 하위 모듈의 `Msg` 컨스트럭터에 접근할 수 있어야만 합니다. `complex-model-centralized-ports` 브랜치의 `Main` 모듈에 정의된 `inboundMethodToMsg` 함수를 살펴봅시다.

```elm
inboundMethodToMsg : Port.InboundMethod -> Msg
inboundMethodToMsg inboundMethod =
    case inboundMethod of
        Port.GotLocalStorageItem kv ->
            kv |> Page.Storage.GotLocalStorageItem >> GotStorageMsg

        Port.GotCookies cookies ->
            cookies |> Page.Storage.GotCookies >> GotStorageMsg

        Port.ConnectedToServer ->
            ConnectedToServer
```

`Main` 모듈이 포트 섭스크립션을 제대로 처리하기 위해 하위 모듈의 `Msg` 타입 컨스트럭터에 직접 접근해야하는 것을 볼 수 있습니다. 이러면 모듈화에 제약이 걸립니다.

또한 중앙 포트 함수는 최대한 유연해야하기 때문에 가장 범용적인 타입인 `Json.Encode.Value`를 사용할 수밖에 없어서 컴파일 타임 타입 안전성이 매우 약해지게 됩니다. 엘름에서 JSON을 디코딩하는 방식 덕분에 런타임에 프로그램이 충돌하는 일이 발생하지는 않지만, 그래도 런타임에 논리 오류가 발생할 가능성은 증가할 수밖에 없습니다. 그리고 디코딩을 처리할 디코더를 만들고 관리보수할 필요도 생기는데, 이는 정말 귀찮고 불편한 작업입니다.

# 어떤 방식을 사용해야 할까

가장 중요한 것은 엘름-자바스크립트 통신이 얼마나 많은가입니다. 별로 없다면 개별 포트 방식이 구현하고 사용하기에 더 간단하고 직관적입니다.

하지만 상당한 양의 엘름-자바스크립트 통신이 필요하다면 다음으로 확인해보아야 할 것은 엘름 모듈 구조가 더 자주 변하는지, 아니면 엘름-자바스크립트 통신 코드가 더 자주변하는지입니다.

엘름 코드가 더 자주 변한다면 엘름 모듈 간 경계를 명확히 할 수 있는 개별 포트 방식이 더 나을 것입니다. 반대의 경우면 엘름과 자바스크립트 영역을 명확히 분리할 수 있는 중앙 포트 방식이 더 나을 것이고요. 코드가 자주 변하는 부분에 더 많은 시간을 쏟기 마련이니 해당 부분의 코드를 고치기 쉽게 코드베이스를 구성하는 것이 합당합니다.

제가 겪은 바로는 엘름 코드는 한 번 작성하면 정말 안정적이라 크게 바꿀 일이 별로 없습니다. 반면 자바스크립트 코드는 언어 및 생태계 특성 상 변화 속도가 매우 빨랐습니다. 그러니 프로젝트가 어떻게 흘러갈지 잘 모르겠으면 장기적으로는 중앙 포트 방식이 더 안전한 선택일 가능성이 높습니다.

두 방식의 장점만 취할 수 있는 방식이 있었으면 좋겠지만 아직 그런 방식을 찾아내지는 못했습니다. 혹시 더 나은 방식이 있다면 공유 부탁드립니다!
