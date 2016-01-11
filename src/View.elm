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
        a


sectionHeader : Model -> Section -> List Html
sectionHeader model section =
    [ tr
        []
        [ th
            [ align "left" ]
            [ a
                [ href <| model.rootUrl ++ section.url
                , target "blank"
                ]
                [ text section.name ]
            ]
        ]
    ]


sectionSeparator : List Html
sectionSeparator =
    [ div [ class "section-separator" ] [] ]


tableRows : Model -> Section -> List Html
tableRows model section =
    case section.procedures of
        [] ->
            []

        procedures ->
            List.concat
                <| [ sectionHeader model section
                   , List.map .html procedures
                   , sectionSeparator
                   ]


getProcHtml : Section -> List Html
getProcHtml section =
    List.map .html (section.procedures)


listSpan : Model -> Html
listSpan model =
    tableflip <| List.take 500 <| List.concat <| List.map (tableRows model) model.sections
