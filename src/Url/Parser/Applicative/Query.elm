module Url.Parser.Applicative.Query exposing
    ( Parser
    , andApp
    , app
    , custom
    , enum
    , int
    , map
    , string
    )

import Dict
import Url.Parser.Applicative.Internal as Q



-- PARSERS


type alias Parser a =
    Q.QueryParser a



-- PRIMITIVES


string : String -> Parser (Maybe String)
string key =
    custom key <|
        \stringList ->
            case stringList of
                [ str ] ->
                    Just str

                _ ->
                    Nothing


int : String -> Parser (Maybe Int)
int key =
    custom key <|
        \stringList ->
            case stringList of
                [ str ] ->
                    String.toInt str

                _ ->
                    Nothing


enum : String -> Dict.Dict String a -> Parser (Maybe a)
enum key dict =
    custom key <|
        \stringList ->
            case stringList of
                [ str ] ->
                    Dict.get str dict

                _ ->
                    Nothing



-- CUSTOM PARSERS


custom : String -> (List String -> a) -> Parser a
custom key func =
    Q.Parser <|
        \dict ->
            func (Maybe.withDefault [] (Dict.get key dict))



-- MAPPING


map : (a -> b) -> Parser a -> Parser b
map func (Q.Parser a) =
    Q.Parser <| \dict -> func (a dict)


app : Parser (a -> b) -> Parser a -> Parser b
app (Q.Parser f) (Q.Parser a) =
    Q.Parser <| \dict -> f dict (a dict)


andApp : Parser a -> Parser (a -> b) -> Parser b
andApp p1 p2 =
    app p2 p1
