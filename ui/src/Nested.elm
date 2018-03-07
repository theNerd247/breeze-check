module Nested exposing (..)


modifyCmd f ( mdl, c ) =
    ( mdl, Cmd.map f c )


modifyMdl f ( mdl, c ) =
    ( f mdl, c )


modifyBoth f g ( mdl, c ) =
    ( f mdl, Cmd.map g c )
