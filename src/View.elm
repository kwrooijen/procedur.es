module View (..) where

import Debug exposing (..)
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


-- procTRs : Model -> Section -> List Html
-- procTRs model section =
--     case section.procedures of
--         [] ->
--           []

--         procedures ->
--             (tr [] [ th [ align "left" ] [ text section.name ] ])
--                 :: (List.map
--                         (\proc ->
--                             tr
--                                 []
--                                 [ procTD model proc.url proc.name
--                                 ]
--                         )
--                         procedures
--                    )


procTRs : Model -> Section -> List Html
procTRs model section =
    case section.procedures of
        [] ->
          []

        procedures ->
            (tr [] [ th [ align "left" ] [ text section.name ] ])
                :: List.map .html procedures

getProcHtml section =
  List.map .html (section.procedures)



listSpan : Model -> Html
listSpan model =
    tableflip <| List.take 500 <| List.concat <| List.map (procTRs model) model.sections
