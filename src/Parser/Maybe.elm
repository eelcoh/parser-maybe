module Parser.Maybe exposing (maybe, withDefault)

import Parser as P exposing (Parser)


{-|


# Parsers

@docs maybe, withDefault

-}



{- Try a parser, and if it fails, return Nothing. -}


maybe : Parser a -> Parser (Maybe a)
maybe p =
    P.oneOf
        [ P.map Just (P.backtrackable p)
        , P.succeed Nothing
        ]



{- Try a parser, and if it fails, return the given default value -}


withDefault : a -> Parser a -> Parser a
withDefault default p =
    P.oneOf
        [ P.backtrackable p
        , P.succeed default
        ]
