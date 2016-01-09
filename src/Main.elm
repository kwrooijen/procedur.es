module Main (..) where

import Debug
import Effects exposing (Never, Effects)
import Html exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import String
import StartApp
import Header exposing (..)
import View exposing (..)
import Updater exposing (..)


port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks


main : Signal Html
main =
    app.html


app : StartApp.App Model
app =
    StartApp.start
        { inputs = []
        , view = view
        , update = Updater.update
        , init = init
        }


init : ( Model, Effects Action )
init =
    ( { queryString = ""
      , procedures = []
      , sections = []
      , defaultProcedures = []
      , defaultSections = []
      , sortBy = .section
      , rootUrl = "https://www.gnu.org/software/guile/manual/html_node/"
      , languages = []
      , language = ""
      }
    , getLanguages
    )


updateFilter : Signal.Address Action -> String -> Signal.Message
updateFilter address v =
    Signal.message address (UpdateQSList v)


view : Signal.Address Action -> Model -> Html
view address model =
    div
        []
        [ View.header
        , div
            [ id "content" ]
            [ div [ id "options" ] []
            , input
                [ id "procedure-input"
                , placeholder "Find Procedure"
                , Attr.value model.queryString
                , on "input" targetValue <| updateFilter address
                ]
                []
            , listSpan model
            ]
        ]
