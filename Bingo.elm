module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode

-- Model
type alias Score = {
        id: Int,
        name: String,
        score: Int
    }

type GameState = EnteringName | Playing
type alias Model = {
    entries : List Entry ,
    gameNumber : Int ,
    name : String,
    alertMessage : Maybe String,
    nameInput : String,
    gameState : GameState
    }
type alias Entry = { id : Int, phrase : String, points : Int, marked : Bool }

initialModel : Model
initialModel =
    {
        name = "",
        gameNumber = 1,
        entries = [],
        alertMessage = Nothing,
        nameInput = "",
        gameState = EnteringName
    }

-- Update

type Msg = NewGame
    | Mark Int
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ChangeGameState state ->
            { model | gameState = state } ! []
        SaveName ->
            if String.isEmpty model.nameInput then
                {model | alertMessage = Just "No name provided." } ! []
            else
                {model | name = model.nameInput, nameInput = "", gameState = Playing } ! []
        CancelName ->
            {model | nameInput = "", gameState = Playing } ! []
        SetNameInput value ->
            {model | nameInput = value} ! []
        NewRandom number ->
            ({model | gameNumber = number}, Cmd.none)
        NewGame ->
            { model | gameNumber = model.gameNumber + 1 } ! [getEntries]
        ShareScore ->
            model ! [postScore model]
        NewScore (Ok score) ->
            let
                msg = "Your score of " ++ (toString score.score) ++ " was successfully shared."
            in
                { model | alertMessage = Just msg } ! []
        NewScore (Err error) ->
            let
                msg = "Error posting score: " ++ (toString error)
            in
                { model | alertMessage = Just msg } ! []
        Mark id ->
            let markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked)}
                    else
                        e
            in
                { model | entries = List.map markEntry model.entries } ! []
        NewEntries (Err error) ->
            let
                errorMessage = case error of
                    Http.NetworkError -> "Is the server running?"
                    Http.BadStatus response -> (toString response.status.message)
                    Http.BadPayload message _ -> "Decoding failed: " ++ message
                    _ -> (toString error)
            in
                { model | alertMessage = Just errorMessage } ! []
        NewEntries (Ok randomEntries) ->
            { model | entries = randomEntries } ! []
        CloseAlert ->
            { model | alertMessage = Nothing } ! []


-- Decoders

entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4 Entry
        (field "id" Decode.int)
        (field "phrase" Decode.string)
        (field "points" Decode.int)
        (succeed False)

scoreEncoder : Model -> Encode.Value
scoreEncoder model =
    Encode.object [
        ("name", Encode.string model.name),
        ("score", Encode.int (sumMarkedPoints model.entries) )
    ]

scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)

-- Commands
generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)

entriesUrl: String
entriesUrl = "http://localhost:3000/random-entries"
postUrl: String
postUrl = "http://localhost:3000/scores"

getEntries: Cmd Msg
getEntries =
    (Decode.list entryDecoder)
        |> Http.get entriesUrl
        |> Http.send NewEntries

postScore: Model -> Cmd Msg
postScore model =
    let
        body = scoreEncoder model |> Http.jsonBody
        request = Http.post postUrl body scoreDecoder
    in
        Http.send NewScore request

-- View

playerInfo: String -> Int -> String
playerInfo name gameNumber =
    "GM: " ++ name ++ " - Game #" ++ (toString gameNumber)

viewPlayer: String -> Int -> Html Msg
viewPlayer name gameNumber =
        h2 [ id "info", class "classy" ]
        [
            a [ href "#", onClick (ChangeGameState EnteringName)] [text name],
            text (" - Game #" ++ (toString gameNumber))
        ]

viewNavigation: Html msg
viewNavigation =
    nav [ class "main-nav" ] [
        a [href "#"] [text "News"],
        a [href "#"] [text "Players"],
        a [href "#"] [text "Schedule"],
        a [href "#"] [text "Notes"]
    ]

viewHeader: String -> Html msg
viewHeader title =
    header [id "header"] [ h1 [] [text title] ]

viewFooter: Html Msg
viewFooter =
    footer [] [ a [href "http://elm-lang.org"] [text "Powered by Elm" ]]


viewEntryItem: Entry -> Html Msg
viewEntryItem entry = li[ classList [("marked", entry.marked)], onClick (Mark entry.id) ] [
                    span [class "phrase"] [text entry.phrase],
                    span [class "points"] [text (toString entry.points)]
                ]

viewEntryList: List Entry -> Html Msg
viewEntryList entries =
    entries |> List.sortBy .points
            |> List.map viewEntryItem
            |> ul []

sumMarkedPoints: List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.map .points
        |> List.sum

viewScore : Int -> Html msg
viewScore score =
    div [class "score"] [
            text "Score: ",
            text (toString score)
        ]

viewDebug: a -> Html msg
viewDebug x = div [class "debug"] [text (toString x)]

hasZeroScore: Model -> Bool
hasZeroScore model =
    (sumMarkedPoints model.entries) == 0

viewNameInput: Model -> Html Msg
viewNameInput model =
    case model.gameState of
        Playing ->
            text ""
        EnteringName ->
            div [class "name-input"]
                [
                    input [ type_ "text", placeholder "Who's playing?", autofocus True, onInput SetNameInput, value model.nameInput ] [],
                    button [ onClick SaveName ] [ text "Save" ],
                    button [ onClick CancelName ] [ text "Cancel" ]
                ]

view: Model -> Html Msg
view model =
    div [ class "content" ]
        [
            viewHeader "Death Blossom Starfinder",
            viewNavigation,
            viewPlayer model.name model.gameNumber,
            viewAlertMessage model.alertMessage,
            viewNameInput model,
            viewEntryList model.entries,
            viewScore (sumMarkedPoints model.entries),
            div [ class "button-group" ]
                [
                    button [ onClick NewGame ] [text "New Game"],
                    button [ onClick ShareScore, disabled (hasZeroScore model) ] [text "Share Score"]
                ],
            viewFooter
        ]
main: Program Never Model Msg
main = Html.program {
        init = (initialModel, getEntries),
        view = view,
        update = update,
        subscriptions = (always Sub.none)
    }

viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
    case alertMessage of
        Just message -> div [class "alert"]
            [
                span [class "close", onClick CloseAlert] [text "X"],
                text message
            ]
        Nothing -> text ""