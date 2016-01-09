module Updater (..) where

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


uGetLanguages model x xs =
    ( { model | languages = (x :: xs) }, getProcedures x )


uGetProcedures model procedures =
    let
        sortedProcedures = List.sortBy model.sortBy procedures
    in
        ( { model
            | procedures = sortedProcedures
            , defaultProcedures = sortedProcedures
          }
        , getSections model.language
        )


uGetSections model sections =
    ( { model
        | sections = sections
        , defaultSections = sections
      }
    , getSections model.language
    )


uUpdateQSList model qs =
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


update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        GetLanguages (Just (x :: xs)) ->
            uGetLanguages model x xs

        GetProcedures (Just procedures) ->
            uGetProcedures model procedures

        GetSections (Just sections) ->
            uGetSections model sections

        UpdateQSList qs ->
            uUpdateQSList model qs

        _ ->
            ( model, Effects.none )


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


containsAll : List String -> Procedure -> Bool
containsAll words procedure =
    let
        hasTerm word =
            String.contains word procedure.name || String.contains word procedure.section
    in
        List.all hasTerm words


filterC : List Procedure -> List Procedure
filterC =
    List.filter (\p -> String.contains "_" p.name)


ignoreC : List Procedure -> List Procedure
ignoreC =
    List.filter (\p -> not <| String.contains "_" p.name)
