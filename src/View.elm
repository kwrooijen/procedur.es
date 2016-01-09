module View (..) where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Header exposing (..)


header : Html
header =
    div
        [ id "header" ]
        [ h1 [] [ text "Procedur.es" ]
        ]


tableHeader : Html
tableHeader =
    tr
        [ class "label" ]
        [ th [ align "left" ] [ text "Procedure" ]
        , th [ align "left" ] [ text "Section" ]
        ]


procTD : Model -> String -> String -> Html
procTD model url proc =
    td
        []
        [ a
            [ href <| model.rootUrl ++ url
            , target "blank"
            ]
            [ text proc ]
        ]


tableflip : List Html -> Html
tableflip a =
    table
        [ id "package-list", style [ ( "width", "100%" ) ] ]
        <| tableHeader
        :: a


procTRs : Model -> Procedure -> Html
procTRs model proc =
    tr
        []
        [ procTD model proc.url proc.name
        , procTD model proc.url proc.section
        ]


listSpan : Model -> Html
listSpan model =
    tableflip <| List.map (procTRs model) model.procedures
