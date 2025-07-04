module Utils exposing
    ( listCount
    , listNub
    )


listNub : List comparable -> List comparable
listNub =
    List.foldr (\x rec -> x :: List.filter (\y -> x /= y) rec) []


listCount : (a -> Bool) -> List a -> Int
listCount f =
    List.foldr
        (\x rec ->
            if f x then
                1 + rec

            else
                rec
        )
        0
