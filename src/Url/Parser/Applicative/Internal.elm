module Url.Parser.Applicative.Internal exposing (QueryParser(..))

import Dict


type QueryParser a
    = Parser (Dict.Dict String (List String) -> a)
