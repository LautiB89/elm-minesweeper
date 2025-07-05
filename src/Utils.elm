module Utils exposing (listCount)


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
