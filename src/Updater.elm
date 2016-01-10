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
import Focus exposing ((=>))


procTD : Model -> ProcedureJson -> Html
procTD model proc =
    tr
        []
        [ td
            []
            [ a
                [ href <| model.rootUrl ++ proc.url
                , target "blank"
                ]
                [ text proc.name ]
            ]
        ]


makeSections =
    List.map (\{ name, url } -> { name = name, url = url, procedures = [] })


uGetLanguages model x xs =
    ( { model | languages = (x :: xs), language = x }, getSections x )


uGetSections model sections =
    let
        s = makeSections sections
    in
        ( { model
            | sections = s
            , defaultSections = s
          }
        , getProcedures model.language
        )


addProcs model procs section =
    let
        sectionProcs = List.filter (\proc -> proc.section == section.name) procs

        procz =
            List.map
                (\p ->
                    { html = (procTD model p)
                    , section = p.section
                    , name = p.name
                    , url = p.url
                    }
                )
                sectionProcs
    in
        { section | procedures = procz }


uGetProcedures model procedures =
    let
        sortedProcedures = List.sortBy model.sortBy procedures

        ok = List.map (addProcs model sortedProcedures) model.sections
    in
        ( { model | sections = ok, defaultSections = ok }
        , Effects.none
        )


uUpdateQSList model qs =
    let
        words = qs |> String.toLower |> String.words

        toFilter =
            if qs == "" then
                model.defaultSections
            else if String.startsWith model.queryString qs then
                model.sections
            else
                model.defaultSections

        ( allSections, _ ) = List.foldr foldso ( [], words ) toFilter
    in
        ( { model | queryString = qs, sections = allSections }, Effects.none )


foldso section ( acc, words ) =
    if sectionContains words section then
        ( section :: acc, words )
    else
        let
            newProcedures = List.filter (containsAll words) section.procedures
        in
            ( { section | procedures = newProcedures } :: acc, words )


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
            case ( String.length qs < 2, (String.endsWith " " qs) && (String.length qs > String.length model.queryString) ) of
                ( _, True ) ->
                    ( { model | queryString = qs }, Effects.none )

                ( True, _ ) ->
                    ( { model | queryString = qs, sections = model.defaultSections }, Effects.none )

                _ ->
                    uUpdateQSList model qs

        _ ->
            Debug.log "Something went wrong.." ( model, Effects.none )


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


decodeProcedures : Json.Decoder (List ProcedureJson)
decodeProcedures =
    Json.object3
        ProcedureJson
        ("section" := Json.string)
        ("name" := Json.string)
        ("url" := Json.string)
        |> Json.list


decodeSections : Json.Decoder (List SectionJson)
decodeSections =
    Json.object2
        SectionJson
        ("name" := Json.string)
        ("url" := Json.string)
        |> Json.list


sectionContains : List String -> Section -> Bool
sectionContains words section =
    let
        name = String.toLower section.name

        hasTerm word =
            String.contains word name
    in
        List.all hasTerm words


containsAll : List String -> Procedure -> Bool
containsAll words procedure =
    let
        hasTerm word =
            String.contains word procedure.name || String.contains word procedure.section
    in
        List.all hasTerm words


updateFilter : Signal.Address Action -> String -> Signal.Message
updateFilter address v =
    Signal.message address (UpdateQSList v)


filterC : List Procedure -> List Procedure
filterC =
    List.filter (\p -> String.contains "_" p.name)


ignoreC : List Procedure -> List Procedure
ignoreC =
    List.filter (\p -> not <| String.contains "_" p.name)
