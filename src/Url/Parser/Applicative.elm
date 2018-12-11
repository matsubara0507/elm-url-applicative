module Url.Parser.Applicative exposing
    ( Parser
    , andApply
    , apply
    , custom
    , fragment
    , ignore
    , int
    , map
    , parse
    , query
    , s
    , string
    )

import Dict exposing (Dict)
import Url exposing (Url)
import Url.Parser.Applicative.Internal as Q
import Url.Parser.Applicative.Query as Query



-- PARSERS


type Parser a
    = Parser (State -> List ( State, a ))


type alias State =
    { visited : List String
    , unvisited : List String
    , params : Dict String (List String)
    , frag : Maybe String
    }



-- PARSE SEGMENTS


string : Parser String
string =
    custom "STRING" Just


int : Parser Int
int =
    custom "NUMBER" String.toInt


s : String -> Parser ()
s str =
    Parser <|
        \{ visited, unvisited, params, frag } ->
            case unvisited of
                [] ->
                    []

                next :: rest ->
                    if next == str then
                        [ ( State (next :: visited) rest params frag, () ) ]

                    else
                        []


custom : String -> (String -> Maybe a) -> Parser a
custom tipe stringToSomething =
    Parser <|
        \{ visited, unvisited, params, frag } ->
            case unvisited of
                [] ->
                    []

                next :: rest ->
                    case stringToSomething next of
                        Just nextValue ->
                            [ ( State (next :: visited) rest params frag, nextValue ) ]

                        Nothing ->
                            []



-- COMBINING PARSERS


map : (a -> b) -> Parser a -> Parser b
map f (Parser parser) =
    Parser <|
        \{ visited, unvisited, params, frag } ->
            List.map (\( state, value ) -> ( state, f value )) <|
                parser <|
                    State visited unvisited params frag


apply : Parser (a -> b) -> Parser a -> Parser b
apply (Parser parser1) parser =
    let
        g ( state, f ) =
            case map f parser of
                Parser parser2 ->
                    parser2 state
    in
    Parser <|
        \{ visited, unvisited, params, frag } ->
            List.concatMap g <| parser1 (State visited unvisited params frag)


andApply : Parser a -> Parser (a -> b) -> Parser b
andApply p1 p2 =
    apply p2 p1


ignore : Parser a -> Parser b -> Parser b
ignore (Parser parser1) (Parser parser2) =
    let
        g =
            \( { visited, unvisited, params, frag }, _ ) ->
                parser2 (State visited unvisited params frag)
    in
    Parser <|
        \{ visited, unvisited, params, frag } ->
            List.concatMap g <| parser1 (State visited unvisited params frag)


oneOf : List (Parser a) -> Parser a
oneOf parsers =
    Parser <|
        \state ->
            List.concatMap (\(Parser parser) -> parser state) parsers


top : Parser ()
top =
    Parser <| \state -> [ ( state, () ) ]



-- QUERY


query : Query.Parser query -> Parser (query -> a) -> Parser a
query (Q.Parser queryParser) parser =
    apply parser <|
        Parser <|
            \{ visited, unvisited, params, frag } ->
                [ ( State visited unvisited params frag, queryParser params ) ]



-- FRAGMENT


fragment : (Maybe String -> fragment) -> Parser (fragment -> a) -> Parser a
fragment toFrag parser =
    apply parser <|
        Parser <|
            \{ visited, unvisited, params, frag } ->
                [ ( State visited unvisited params frag, toFrag frag ) ]



-- PARSE


parse : Parser a -> Url -> Maybe a
parse (Parser parser) url =
    getFirstMatch <|
        parser <|
            State [] (preparePath url.path) (prepareQuery url.query) url.fragment


getFirstMatch : List ( State, a ) -> Maybe a
getFirstMatch states =
    case states of
        [] ->
            Nothing

        ( state, value ) :: rest ->
            case state.unvisited of
                [] ->
                    Just value

                [ "" ] ->
                    Just value

                _ ->
                    getFirstMatch rest



-- PREPARE PATH


preparePath : String -> List String
preparePath path =
    case String.split "/" path of
        "" :: segments ->
            removeFinalEmpty segments

        segments ->
            removeFinalEmpty segments


removeFinalEmpty : List String -> List String
removeFinalEmpty segments =
    case segments of
        [] ->
            []

        "" :: [] ->
            []

        segment :: rest ->
            segment :: removeFinalEmpty rest



-- PREPARE QUERY


prepareQuery : Maybe String -> Dict String (List String)
prepareQuery maybeQuery =
    case maybeQuery of
        Nothing ->
            Dict.empty

        Just qry ->
            List.foldr addParam Dict.empty (String.split "&" qry)


addParam : String -> Dict String (List String) -> Dict String (List String)
addParam segment dict =
    case String.split "=" segment of
        [ rawKey, rawValue ] ->
            case Url.percentDecode rawKey of
                Nothing ->
                    dict

                Just key ->
                    case Url.percentDecode rawValue of
                        Nothing ->
                            dict

                        Just value ->
                            Dict.update key (addToParametersHelp value) dict

        _ ->
            dict


addToParametersHelp : a -> Maybe (List a) -> Maybe (List a)
addToParametersHelp value maybeList =
    case maybeList of
        Nothing ->
            Just [ value ]

        Just list ->
            Just (value :: list)
