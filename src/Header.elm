module Header (..) where


type Action
    = None
    | GetProcedures (Maybe (List Procedure))
    | GetSections (Maybe (List Section))
    | GetLanguages (Maybe (List String))
    | UpdateQSList String


type alias Model =
    { queryString : String
    , procedures : List Procedure
    , sections : List Section
    , defaultProcedures : List Procedure
    , defaultSections : List Section
    , sortBy : Procedure -> String
    , rootUrl : String
    , languages : List String
    , language : String
    }


type alias Section =
    { name : String
    , url : String
    }


type alias Procedure =
    { section : String
    , name : String
    , url : String
    }
