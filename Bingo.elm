module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

-- Model
type alias Model = { entries : List Entry , gameNumber : Int , name : String }
type alias Entry = { id : Int, phrase : String, points : Int, marked : Bool }

initialModel : { entries : List Entry , gameNumber : Int , name : String }
initialModel =
    {
        name = "Jason",
        gameNumber = 1,
        entries = initialEntries
    }

initialEntries : List Entry
initialEntries =
    [
        Entry 1 "Future-Proof" 100 False,
        Entry 2 "Doing Agile" 200 False,
        Entry 3 "Wat?" 300 False,
        Entry 4 "What Did He Type" 400 False
    ]

-- View

playerInfo: String -> Int -> String
playerInfo name gameNumber =
    "GM: " ++ name ++ " - Game #" ++ (toString gameNumber)

viewPlayer: String -> Int -> Html msg
viewPlayer name gameNumber =
    let
        playerInfoText name gameNumber =
            playerInfo name gameNumber
            |> String.toUpper
            |> text
    in
        h2 [ id "info", class "classy" ]
            [ playerInfoText name gameNumber ]

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

viewFooter: Html msg
viewFooter =
    footer [] [ a [href "http://elm-lang.org"] [text "Powered by Elm" ]]

viewInputField: String -> String -> Html msg
viewInputField attrName attrType =
    div[class "inputField"] [
        label [ for (String.toLower attrName) ] [text (attrName ++ ": ")],
        input [ id (String.toLower attrName), type_ attrType, name attrName] []
     ]

viewInputText: String -> Html msg
viewInputText name =
    viewInputField name "text"

viewInputPassword: String -> Html msg
viewInputPassword name =
    viewInputField name "password"

viewFormDoubleButtons: String -> String -> Html msg
viewFormDoubleButtons button1 button2 =
    div[class "buttonField"] [
        button [name (String.toLower button1)] [text button1],
        button [name (String.toLower button2)] [text button2]
    ]

viewRegLoginForm: Html msg
viewRegLoginForm =
    Html.form [method "post"] [
        h2 [] [text "Register & Login"],
        viewInputText "Username",
        viewInputPassword "Password",
        hr [] [],
        viewFormDoubleButtons "Register" "Log In"
    ]

view: Model -> Html msg
view model =
    div [ class "content" ]
        [
            viewHeader "Death Blossom Starfinder",
            viewNavigation,
            viewPlayer model.name model.gameNumber,
            div [ class "debug" ] [text (toString model)],
            viewRegLoginForm,
            viewFooter
        ]

main: Html msg
main = view initialModel