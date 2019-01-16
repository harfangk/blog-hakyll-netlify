---
title: How to Trigger Multiple Msgs in Elm
---

In the Elm Architecture, `update` function is responsible for changing `Model` state. Depending on how you structure your `Model`, `Msg`, and `update`, sometimes you may want to call `update` function again with another `Msg` after calling `update` function. Recursively calling `update` is straightforward.

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

But sometimes you may want to trigger multiple `Msg`s. `Task` can be used here. 

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

The caveat is that there's no guaranteed ordering between `SecondMsg` and `ThirdMsg`, and that these subsequent `Msg`s require arguments. Most importantly, I think recursively calling `update` is a bad practice. Use this approach only when there's absolutely no other way.
