module Header (..) where

import Html exposing (..)


type Action
    = None
    | GetProcedures (Maybe (List ProcedureJson))
    | GetSections (Maybe (List SectionJson))
    | GetLanguages (Maybe (List String))
    | UpdateQSList String


type alias Model =
    { queryString : String
    , sections : List Section
    , defaultSections : List Section
    , sortBy : ProcedureJson -> String
    , rootUrl : String
    , languages : List String
    , language : String
    }


type alias Section =
    { name : String
    , url : String
    , procedures : List Procedure
    }


type alias Procedure =
    { section : String
    , name : String
    , url : String
    , html : Html
    , description : String
    }


type alias SectionJson =
    { name : String
    , url : String
    }


type alias ProcedureJson =
    { section : String
    , name : String
    , url : String
    , description : String
    }
