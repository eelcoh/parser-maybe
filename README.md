Parsers for parsing optional values.

```elm

import Parser exposing (Parser, (|.), (|=), succeed, symbol, float, spaces)
import Parser.Maybe as M


type alias Point =
  { x : Float
  , y : Float
  }


point : Parser Point
point =
  succeed Point
    |. symbol "("
    |. spaces
    |= float
    |. spaces
    |. symbol ","
    |. spaces
    |= float
    |. spaces
    |. symbol ")"


optionalPoint : Parser (Maybe Point)
optionalPoint =
  M.maybe point


defaultPoint : Parser (Maybe Point)
defaultPoint =
  M.withDefault (0, 0) point

```

**Note:** This parser makes use of elm/parser's `backtrackable`, which may have performance impact.
