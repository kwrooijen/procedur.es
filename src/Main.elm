module Main where

import Effects exposing (Never, Effects)
import Html exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import String
import StartApp
import Regex exposing (regex, caseInsensitive)

header =
  div [ id "header" ]
        [ h1 [] <| [text "Procedur.es"]
        ]

listSpan model =
    table [ id "package-list", style [("width", "100%")]] <|
    tr [ class "label" ]
       [ th [align "left"] [text "Procedure"]
       , th [align "left"] [text "Section"]
       ] ::
    List.map (\proc ->
        tr [ ]
           [ td [] [ a [ href <| model.rootUrl ++ proc.procedureUrl
                   , target "blank" ]
                   [ text proc.procedure ]
                   ]
           , td [] [ a [ href <| model.rootUrl ++ proc.sectionUrl
                   , target "blank" ]
                   [ text proc.section ]
                   ]
          ]
    ) model.procedureList

view address model =
    div []
        [ header
        , div [ id "content" ]
              [ div [id "options"] []
              , input
                  [ id "procedure-input"
                  , placeholder "Find Procedure"
                  , Attr.value model.queryString
                  , on "input" targetValue <| updateFilter address
                  ] []
              ,  listSpan model
              ]
        ]

updateFilter address v =
    (Signal.message address (UpdateQSList v))

type Action
  = None
  | GetList (Maybe (List Procedure))
  | UpdateQSList String

update action model =
  case action of
      GetList (Just list) ->
          let sortedList = ignoreC <| List.sortBy model.sortBy list in
          ({model | procedureList = sortedList, defaultList = sortedList}, Effects.none)
      UpdateQSList qs ->
        let strings = String.split " " qs
            toFilter = if String.startsWith model.queryString qs
                       then model.procedureList else model.defaultList
            newProcedureList = toFilter |> List.filter (containsAll strings)
        in ({model | queryString = qs, procedureList = newProcedureList}, Effects.none)
      _ -> (model, Effects.none)

filterC = List.filter (\p -> String.contains "_" p.procedure)
ignoreC = List.filter (\p -> not <| String.contains "_" p.procedure)

containsAll strings item =
  (List.all (\s ->
      Regex.contains (s |> regex |> caseInsensitive) <| item.procedure ++ item.section)
   strings)

main = app.html

app =
    StartApp.start
        { inputs = []
        , view = view
        , update = update
        , init = init "guile"
        }

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

type alias Model =
    { queryString : String
    , procedureList : List Procedure
    , defaultList : List Procedure
    , sortBy : (Procedure -> String)
    , rootUrl : String
    }

init : String -> (Model, Effects Action)
init language =
  ( Model "" [] [] .section "https://www.gnu.org/software/guile/manual/html_node/"
  , getProcedures language
  )

getProcedures : String -> Effects Action
getProcedures language =
  Http.get decodeUrl (procedureUrl language)
    |> Task.toMaybe
    |> Task.map GetList
    |> Effects.task

procedureUrl : String -> String
procedureUrl language =
 "gen/json/" ++ language ++ "-procs.json"

type alias Procedure =
  { section : String
  , procedure : String
  , sectionUrl : String
  , procedureUrl : String
  }

decodeUrl : Json.Decoder (List Procedure)
decodeUrl =
    Json.object4 Procedure
        ("section"       := Json.string)
        ("procedure"     := Json.string)
        ("section-url"   := Json.string)
        ("procedure-url" := Json.string) |> Json.list
