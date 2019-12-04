module Day4 exposing (part1Output, part2Output)


iterate : Int -> Int -> List Int -> List Int
iterate low high currentList =
    let
        pairs =
            low |> String.fromInt |> String.toList |> pairwise
    in
    if low >= high then
        currentList

    else if List.any (\( a, b ) -> a == b) pairs && List.all (\( a, b ) -> a <= b) pairs then
        iterate (low + 1) high (low :: currentList)

    else
        iterate (low + 1) high currentList


part1Output =
    iterate 134564 585159 [] |> List.length |> String.fromInt


pairwise : List a -> List ( a, a )
pairwise list =
    pairwiseHelper list [] |> List.reverse


pairwiseHelper : List a -> List ( a, a ) -> List ( a, a )
pairwiseHelper remaining newList =
    case remaining of
        a :: b :: rest ->
            pairwiseHelper (b :: rest) (( a, b ) :: newList)

        _ ->
            newList


pairwise3 : List a -> List ( a, a, a )
pairwise3 list =
    pairwiseHelper3 list [] |> List.reverse


pairwiseHelper3 : List a -> List ( a, a, a ) -> List ( a, a, a )
pairwiseHelper3 remaining newList =
    case remaining of
        a :: b :: c :: rest ->
            pairwiseHelper3 (b :: c :: rest) (( a, b, c ) :: newList)

        _ ->
            newList


iteratePart2 : Int -> Int -> List Int -> List Int
iteratePart2 low high currentList =
    let
        pairs =
            low |> String.fromInt |> String.toList |> pairwise

        pairs3 =
            low |> String.fromInt |> String.toList |> pairwise3
    in
    if low >= high then
        currentList

    else if
        List.all (\( a, b ) -> a <= b) pairs
            && List.any (\( a, b, c ) -> (a == b || b == c) && (a /= c)) pairs3
    then
        case low |> String.fromInt |> String.toList of
            a :: b :: c :: d :: e :: f :: [] ->
                if
                    (a == b && a /= c)
                        || (b == c && b /= d && a /= c)
                        || (c == d && c /= e && b /= d)
                        || (d == e && d /= f && c /= e)
                        || (e == f && d /= f)
                then
                    iteratePart2 (low + 1) high (low :: currentList)

                else
                    iteratePart2 (low + 1) high currentList

            _ ->
                iteratePart2 (low + 1) high currentList

    else
        iteratePart2 (low + 1) high currentList


part2Output : String
part2Output =
    iteratePart2 134564 585159 [] |> List.length |> String.fromInt
