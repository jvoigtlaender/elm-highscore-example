module Main (main) where

import Dict exposing (Dict)
import ElmFire
import ElmFire.Dict
import ElmFire.Op
import Html exposing (Attribute, Html, button, div, input, li, ol, text)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (on, onClick, targetValue)
import Json.Decode
import Json.Encode
import Signal exposing (Address, Mailbox)
import String
import Task exposing (Task)


url =
    "https://highscore-example.firebaseio.com/"


database =
    { location = ElmFire.fromUrl url
    , orderOptions = ElmFire.noOrder
    , encoder = Json.Encode.int
    , decoder = Json.Decode.int
    }


type Action
    = NoOp
    | Dict (Dict String Int)
    | Name String
    | Score Int
    | SubmitAndFetch


actions : Mailbox Action
actions =
    Signal.mailbox NoOp


fetchDict : Task ElmFire.Error ()
fetchDict =
    ElmFire.Dict.getDict database `Task.andThen` (Dict >> Signal.send actions.address)


adjustDict : String -> Int -> Task ElmFire.Error ElmFire.Reference
adjustDict name score =
    ElmFire.Op.operate database (ElmFire.Op.update name (Just << Maybe.withDefault score << Maybe.map (max score)))


type alias Model =
    { dict : Dict String Int, name : String, score : Int }


update : Action -> ( Model, x ) -> ( Model, Maybe (Task ElmFire.Error ()) )
update action ( model, _ ) =
    case action of
        NoOp ->
            ( model, Nothing )

        Dict dict ->
            ( { model | dict = dict }, Nothing )

        Name name ->
            ( { model | name = name }, Nothing )

        Score score ->
            ( { model | score = score }, Nothing )

        SubmitAndFetch ->
            ( model, Just (adjustDict model.name model.score `Task.andThen` always fetchDict) )


onInput : Address a -> (String -> a) -> Attribute
onInput addr fun =
    on "input" targetValue (Signal.message addr << fun)


view : Model -> Html
view { dict } =
    div
        []
        [ input [ placeholder "Name", onInput actions.address Name ] []
        , input [ placeholder "Score", onInput actions.address (Score << Result.withDefault 0 << String.toInt) ] []
        , button [ onClick actions.address SubmitAndFetch ] [ text "Submit and fetch" ]
        , ol [] (List.map (\pair -> li [] [ text (toString pair) ]) (List.reverse (List.sortBy snd (Dict.toList dict))))
        ]


modelAndTasks : Signal ( Model, Maybe (Task ElmFire.Error ()) )
modelAndTasks =
    Signal.foldp update ( { dict = Dict.empty, name = "", score = 0 }, Just fetchDict ) actions.signal


main : Signal Html
main =
    Signal.map (view << fst) modelAndTasks


port run : Signal (Task ElmFire.Error ())
port run =
    Signal.filterMap snd (Task.succeed ()) modelAndTasks
