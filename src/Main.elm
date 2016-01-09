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


updateFilter : Signal.Address Action -> String -> Signal.Message
updateFilter address v =
    (Signal.message address (UpdateQSList v))


update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        GetLanguages (Just (x :: xs)) ->
            ( { model | languages = (x :: xs) }, getProcedures x )

        GetProcedures (Just procedures) ->
            let
                sortedProcedures = List.sortBy model.sortBy procedures
            in
                ( { model | procedures = sortedProcedures, defaultProcedures = sortedProcedures }, getSections model.language )

        GetSections (Just sections) ->
            ( { model | sections = sections, defaultSections = sections }, getSections model.language )

        UpdateQSList qs ->
            let
                words = qs |> String.toLower |> String.words

                toFilter =
                    if String.startsWith model.queryString qs then
                        model.procedures
                    else
                        model.defaultProcedures

                newProcedureList = toFilter |> List.filter (containsAll words)
            in
                ( { model | queryString = qs, procedures = newProcedureList }, Effects.none )

        _ ->
            ( model, Effects.none )


filterC : List Procedure -> List Procedure
filterC =
    List.filter (\p -> String.contains "_" p.name)


ignoreC : List Procedure -> List Procedure
ignoreC =
    List.filter (\p -> not <| String.contains "_" p.name)


containsAll : List String -> Procedure -> Bool
containsAll words procedure =
    let
        hasTerm word =
            String.contains word procedure.name || String.contains word procedure.section
    in
        List.all hasTerm words


main : Signal Html
main =
    app.html


app : StartApp.App Model
app =
    StartApp.start
        { inputs = []
        , view = view
        , update = update
        , init = init
        }


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


port tasks : Signal (Task.Task Never ())
port tasks =
    app.tasks


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


getLanguages : Effects Action
getLanguages =
    Http.get decodeLanguages "gen/json/languages.json"
        |> Task.toMaybe
        |> Task.map GetLanguages
        |> Effects.task


getProcedures : String -> Effects Action
getProcedures language =
    Http.get decodeProcedures (procedureUrl language)
        |> Task.toMaybe
        |> Task.map GetProcedures
        |> Effects.task


getSections : String -> Effects Action
getSections language =
    Http.get decodeSections (sectionUrl language)
        |> Task.toMaybe
        |> Task.map GetSections
        |> Effects.task


procedureUrl : String -> String
procedureUrl language =
    "gen/json/" ++ language ++ "-procs.json"


sectionUrl : String -> String
sectionUrl language =
    "gen/json/" ++ language ++ "-sections.json"


decodeLanguages : Json.Decoder (List String)
decodeLanguages =
    Json.list Json.string


decodeProcedures : Json.Decoder (List Procedure)
decodeProcedures =
    Json.object3
        Procedure
        ("section" := Json.string)
        ("name" := Json.string)
        ("url" := Json.string)
        |> Json.list


decodeSections : Json.Decoder (List Section)
decodeSections =
    Json.object2
        Section
        ("name" := Json.string)
        ("url" := Json.string)
        |> Json.list
