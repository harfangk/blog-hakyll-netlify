---
title: How to Organize Port Functions in Elm
---

I've been thinking about how to organize Elm's port functions and here's two approaches that I've put together. I assume that the readers know basics of Elm, including the Elm Architecture and how ports work. I put a working Elm app example in my Github repository. If you want to run it, follow this instruction to set it up:

<!--more-->

```bash
git clone harfangk/elm-port-examples
cd elm-port-example
npm install --global elm elm-live
elm-live src/Main.elm --open -- --output=elm.js
```

# Individual Ports

This approach defines a port function for each interaction between Elm and JavaScript. Since ports are always one-way, if data goes back and forth, that requires a pair of ports. Here's a sample implementation. The entire code can be found in `individual-ports` branch.

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

Individual ports approach lets you write more granular functions that do only one things. They also have more specific type signatures for inputs and outputs, providing better type safety at compile time. Since they are all separate functions, you can give them semantic names for better readability. 

Because port functions are small and specific, they are unlikely to leak across module boundaries. Check out `complex-model-individual-ports` branch for a more complex model structure. You can see that `Page.Storage.initModel`, `Page.Storage.update`, `Page.Storage.view`, and `Page.Storage.subscriptions` are the only connecting parts between `Main` module and `Page.Storage` module. This makes it easy to decouple modules and replace them as required.

On the other hand, the number of required port functions quickly grows unwieldy under this approach - 10 port functions were required to implement basic local storage and cookie interactions. It's not hard to see more than a hundred port functions in larger applications.

Moreover, while this approach allows easier modularization, each module tends to intermix core Elm codes and Elm-JavaScript interoperation codes. In the `Page.Storage` module, it is hard to quickly discern which `Msg` handles Elm events and which handles JavaScript events. You can use comments to somewhat mitigate this, but this remedy is likely to fall apart as you add more `Msg`.

Lastly, each port function requires its own event listener, which might tax performance at some point. But I'm not sure how Elm handles this issue so it might be an unfounded concern.

## Centralized Ports

This approach defines one port to handle all interactions from Elm to JavaScript, and another to handle all interactions from JavaScript to Elm. The entire code can be found in `centralized-ports` branch.

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

Centralized ports approach uses two port functions that act as routers between Elm and JavaScript. As everything about Elm-JavaScript interoperation is handled by these two functions, it is easy to concentrate all interoperation codes into a particular module. This provides a clear separation of concerns between Elm codes and Elm-JavaScript interoperation codes, making it easier to understand and modify that particular domain of the application. If you want to interact with JavaScript code, you can simply call functions defined in the `Port` module instead of encoding and decoding data for port functions over and over again; if you want to modify port functions, you can simply change codes in that one module and be done with it. 

But this approach makes modularization a bit hard. Since all port subscriptions must be assigned a `Msg` to handle them at the top level `Main` module, it needs to know about the `Msg` constructors of its submodules. Here's `inboundMethodToMsg` function in the `Main` module in the `complex-model-centralized-ports` branch for an example. 

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

In this implementation the `Main` module has to know about specific constructors of its submodules' `Msg` types to correctly handle port subscriptions, undermining modularization. 

Moreover, the two port functions have the most generic type signatures, `Json.Encode.Value`, in order to be so flexible. This cripples compile time type safety for them. There will still be no runtime crash thanks to how JSON decoding works in Elm, but this does increase the chance of logic bugs at runtime. And it also means that you'll have to write and maintain that decoder, which is one of the less pleasant aspects of Elm.

# So When to Use Which?

The first question to ask is how large your Elm-JavaScript interoperation would be. If it's not going to be much, individual ports approach is simpler and more intuitive to implement.

But if your application is going to have nontrivial amount of interoperation, then this question helps: between the structure of your Elm modules and Elm-JavaScript interoperation codes, which changes more quickly in your application?

If your Elm code changes more quickly, then individual ports approach might be better because of clean boundary among Elm modules. Otherwise, centralized ports approach might be better because of clean boundary between domains. It always makes sense to have quickly changing parts easier to modify, because that's where you'll be spending most of your time. 

My experience is that once written, Elm codes are extremely sturdy and require little change. On the other hand, JavaScript codes tend to change often because of the inherent language and ecosystem characteristics. So if you aren't sure, centralized ports approach would be a safer bet in the long run.

I wish there was another way that had only the benefits of both approaches, but couldn't come up with it. If you know a better approach, please tell me about it!
