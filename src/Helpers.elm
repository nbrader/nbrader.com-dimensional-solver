module Helpers exposing (..)

type alias Pair a = (a, Int)

permutationsOfNums : List number -> List (List number)
permutationsOfNums list =
    permutationsHelper 
        (groupSort list 
            |> List.map 
                (\group2 -> 
                    (Maybe.withDefault 0 (List.head group2), List.length group2)
                )
        )

permutationsHelper : List (Pair a) -> List (List a)
permutationsHelper pairs =
    case pairs of
        [] ->
            [ [] ]

        _ ->
            List.concatMap (\( (val, _), xs ) -> List.map (\ys -> val :: ys) (permutationsHelper xs)) (select pairs)

groupSort : List comparable -> List (List comparable)
groupSort list =
    list |> List.sort |> group

group : List a -> List (List a)
group list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                (ys, zs) =
                    span (\y -> y == x) xs
            in
            (x :: ys) :: group zs

span : (a -> Bool) -> List a -> (List a, List a)
span predicate list =
    spanHelp predicate [] list

spanHelp : (a -> Bool) -> List a -> List a -> (List a, List a)
spanHelp predicate acc list =
    case list of
        [] ->
            (List.reverse acc, [])

        x :: xs ->
            if predicate x then
                spanHelp predicate (x :: acc) xs
            else
                (List.reverse acc, x :: xs)

select : List (Pair a) -> List (Pair a, List (Pair a))
select pairs =
    case pairs of
        [] ->
            []

        x :: xs ->
            let
                (val, n) = x
                xsNew =
                    if n == 1 then
                        xs
                    else
                        (val, n - 1) :: xs
            in
            (x, xsNew) :: List.map (\(y, cs) -> (y, x :: cs)) (select xs)

subsets : List a -> List (List a)
subsets list =
    case list of
        [] ->
            [ [] ]

        x :: xs ->
            let
                subs =
                    subsets xs
            in
            List.concatMap (\sub -> [ x :: sub, sub ]) subs

headTail : List a -> Maybe (a, List a)
headTail list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (x, xs)

delete : a -> List a -> List a
delete x list =
    List.filter (\y -> y /= x) list