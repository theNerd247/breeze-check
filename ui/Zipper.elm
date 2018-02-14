module Zipper exposing (..)


type alias HasZipper m a =
    { m
        | before : List a
        , selected : a
        , after : List a
    }


type alias ZipperBase a =
    { before : List a
    , selected : a
    , after : List a
    }


next : HasZipper m a -> HasZipper m a
next z =
    case z.after of
        [] ->
            z

        x :: xs ->
            { z | before = z.selected :: z.before, selected = x, after = xs }


prev : HasZipper m a -> HasZipper m a
prev z =
    case z.before of
        [] ->
            z

        x :: xs ->
            { z | before = xs, selected = x, after = z.selected :: z.after }


fromList : a -> List a -> ZipperBase a
fromList x xs =
    { before = [], selected = x, after = xs }
