module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (..)

import Helpers exposing (..)

type alias BaseUnits = String
type alias DimExpr = (Expr Float, Expr BaseUnits)
type alias Pair a = (a, Int)

type Expr a
    = Const a
    | Add (Expr a) (Expr a)
    | Sub (Expr a) (Expr a)
    | Mult (Expr a) (Expr a)
    | Div (Expr a) (Expr a)

type alias NumersDenoms = (List BaseUnits, List BaseUnits)

allDimExprs : List DimExpr -> List DimExpr
allDimExprs input =
    let
        newArgs = List.concatMap permutationsOfDimExprs (subsets input)

        go : List DimExpr -> List DimExpr
        go x = case x of 
            [] ->
                []

            [ arg ] ->
                [ arg ]

            args ->
                let
                    n = List.length args
                in
                List.concatMap
                    (\i ->
                        let
                            leftArgs = go (List.take i args)
                            rightArgs = go (List.drop i args)
                        in
                        List.concatMap
                            (\(l, lUnits) ->
                                List.concatMap
                                    (\(r, rUnits) ->
                                        [ (Add l r, Add lUnits rUnits)
                                        , (Sub l r, Sub lUnits rUnits)
                                        , (Mult l r, Mult lUnits rUnits)
                                        , (Div l r, Div lUnits rUnits)
                                        ]
                                    )
                                    rightArgs
                            )
                            leftArgs
                    )
                    (List.range 1 (n - 1))
    in
    List.concatMap go newArgs

-- Bidirectional mapping between DimExpr and Int
dimExprToInt : List DimExpr -> DimExpr -> Int
dimExprToInt dimExprList dimExpr =
    case elemIndex dimExpr dimExprList of
        Just index -> index
        Nothing -> -1  -- This should not happen if the DimExpr is in the list

intToDimExpr : List DimExpr -> Int -> DimExpr
intToDimExpr dimExprList int =
    List.Extra.getAt int dimExprList |> Maybe.withDefault (Const 0.0, Const "Dimensionless")

permutationsOfDimExprs : List DimExpr -> List (List DimExpr)
permutationsOfDimExprs dimExprs =
    let
        -- Convert DimExpr values to integers
        intList = List.map (dimExprToInt dimExprs) dimExprs

        -- Generate permutations of integers
        intPermutations = permutationsOfNums intList
    in
    -- Convert permutations of integers back to DimExpr values
    List.map (List.map (intToDimExpr dimExprs)) intPermutations

eval : Expr Float -> Maybe Float
eval expr =
    case expr of
        Const x ->
            Just x

        Add e1 e2 ->
            Maybe.map2 (+) (eval e1) (eval e2)

        Sub e1 e2 ->
            Maybe.map2 (-) (eval e1) (eval e2)

        Mult e1 e2 ->
            Maybe.map2 (*) (eval e1) (eval e2)

        Div e1 e2 ->
            case (eval e1, eval e2) of
                (Just numerator, Just denominator) ->
                    if denominator == 0.0
                     then Nothing
                     else Just (numerator / denominator)

                _ -> 
                    Nothing

showFloatExpr : Expr Float -> String
showFloatExpr expr =
    case expr of
        Const x ->
            String.fromFloat x

        Add e1 e2 ->
            "(" ++ showFloatExpr e1 ++ " + " ++ showFloatExpr e2 ++ ")"

        Sub e1 e2 ->
            "(" ++ showFloatExpr e1 ++ " - " ++ showFloatExpr e2 ++ ")"

        Mult e1 e2 ->
            "(" ++ showFloatExpr e1 ++ " * " ++ showFloatExpr e2 ++ ")"

        Div e1 e2 ->
            "(" ++ showFloatExpr e1 ++ " / " ++ showFloatExpr e2 ++ ")"

showUnits : Expr BaseUnits -> String
showUnits expr =
    case expr of
        Const x ->
            showBaseUnits x

        Add e1 e2 ->
            "(" ++ showUnits e1 ++ " + " ++ showUnits e2 ++ ")"

        Sub e1 e2 ->
            "(" ++ showUnits e1 ++ " - " ++ showUnits e2 ++ ")"

        Mult e1 e2 ->
            "(" ++ showUnits e1 ++ " * " ++ showUnits e2 ++ ")"

        Div e1 e2 ->
            "(" ++ showUnits e1 ++ " / " ++ showUnits e2 ++ ")"

showBaseUnits : BaseUnits -> String
showBaseUnits units = units

normaliseUnits : Expr BaseUnits -> Maybe (Expr BaseUnits)
normaliseUnits expr =
    case toNumersDenoms expr of
        Just nd -> Just (fromNumersDenoms nd)
        Nothing -> Nothing

toNumersDenoms : Expr BaseUnits -> Maybe NumersDenoms
toNumersDenoms expr =
    case expr of
        Const x ->
            Just ([x], [])

        Add e1 e2 ->
            case (toNumersDenoms e1, toNumersDenoms e2) of
                (Just nd1, Just nd2) ->
                    if nd1 == nd2 then
                        Just nd1
                    else
                        Nothing

                _ ->
                    Nothing

        Sub e1 e2 ->
            toNumersDenoms (Add e1 e2)

        Mult e1 e2 ->
            let
                process (numers1, denoms1) (numers2, denoms2) =
                    let
                        numers = List.filter (\x -> x /= "Dimensionless") (numers1 ++ numers2)
                        denoms = List.filter (\x -> x /= "Dimensionless") (denoms1 ++ denoms2)
                        numersNew = List.foldr delete numers denoms
                        denomsNew = List.foldr delete denoms numers
                    in
                    (numersNew, denomsNew)
            in
            case (toNumersDenoms e1, toNumersDenoms e2) of
                (Just nd1, Just nd2) ->
                    Just (process nd1 nd2)

                _ ->
                    Nothing

        Div e1 e2 ->
            let
                process (numers1, denoms1) (numers2, denoms2) =
                    let
                        numers = List.filter (\x -> x /= "Dimensionless") (numers1 ++ denoms2)
                        denoms = List.filter (\x -> x /= "Dimensionless") (denoms1 ++ numers2)
                        numersNew = List.foldr delete numers denoms
                        denomsNew = List.foldr delete denoms numers
                    in
                    (numersNew, denomsNew)
            in
            case (toNumersDenoms e1, toNumersDenoms e2) of
                (Just nd1, Just nd2) ->
                    Just (process nd1 nd2)

                _ ->
                    Nothing

fromNumersDenoms : NumersDenoms -> Expr BaseUnits
fromNumersDenoms (numers, denoms) =
    let 
        numer = multiply numers
        denom = multiply denoms

        multiply : List BaseUnits -> Expr BaseUnits
        multiply list =
            case list of
                [] ->
                    Const "Dimensionless"

                [x] ->
                    Const x

                x :: xs ->
                    Mult (Const x) (multiply xs)
    in if denom == Const "Dimensionless" then
           numer
       else
           Div numer denom

normaliseDimExprs : List DimExpr -> List (Maybe (Expr Float, Expr BaseUnits))
normaliseDimExprs exprs = 
    List.map (\(x, units) -> case normaliseUnits units of
                               Just normalized -> Just (x, normalized)
                               Nothing -> Nothing) exprs

removeSndFailures : List (Maybe (Expr Float, Expr BaseUnits)) -> List DimExpr
removeSndFailures = 
    List.filterMap identity

showDimExpr : DimExpr -> String
showDimExpr (x, units) = 
    String.fromFloat (Maybe.withDefault 0 (eval x)) ++ "\t" ++ showFloatExpr x ++ "\t" ++ showUnits units

printDimExprs : List DimExpr -> List String
printDimExprs =
    List.map showDimExpr

printAllDimExprs : List DimExpr -> List String
printAllDimExprs args =
    printDimExprs <| removeSndFailures <| normaliseDimExprs args

printMatchingDimExprs : Float -> List DimExpr -> List String
printMatchingDimExprs target args =
    allDimExprs args
        |> List.filter (\(expr, _) -> eval expr == Just target)
        |> normaliseDimExprs
        |> removeSndFailures
        |> printDimExprs

printNearDimExprs : Float -> Float -> List DimExpr -> List String
printNearDimExprs target tol args =
    allDimExprs args
        |> List.filter (\(expr, _) -> case eval expr of
                                       Just val -> abs (val - target) <= tol
                                       Nothing -> False)
        |> normaliseDimExprs
        |> removeSndFailures
        |> printDimExprs

type Mode
    = Exact
    | Approximate

type alias Model =
    { inputData : String
    , targetValue : String
    , toleranceValue : String
    , mode : Mode
    , output : String
    }

type Msg
    = InputChanged String
    | TargetChanged String
    | ToleranceChanged String
    | ToggleMode
    | SubmitInput

init : () -> (Model, Cmd Msg)
init _ =
    (
        { inputData = "1.0, Dimensionless\n1.0, Dimensionless\n1.0, TimeHours\n1.0, TimeHours"
        , targetValue = "2.0"
        , toleranceValue = "0.1"
        , mode = Exact
        , output = ""
        },
        Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InputChanged newInput ->
            ({ model | inputData = newInput }, Cmd.none)

        TargetChanged newTarget ->
            ({ model | targetValue = newTarget }, Cmd.none)

        ToleranceChanged newTolerance ->
            ({ model | toleranceValue = newTolerance }, Cmd.none)

        ToggleMode ->
            let
                newMode =
                    case model.mode of
                        Exact -> Approximate
                        Approximate -> Exact
            in
            ({ model | mode = newMode }, Cmd.none)

        SubmitInput ->
            let
                target = String.toFloat model.targetValue |> Maybe.withDefault 0.0
                tolerance = String.toFloat model.toleranceValue |> Maybe.withDefault 0.1
                dimExprs = String.lines model.inputData |> List.map parseDimExpr
                results = 
                    case model.mode of
                        Exact -> printMatchingDimExprs target dimExprs
                        Approximate -> printNearDimExprs target tolerance dimExprs
                outputText = String.join "\n" results
            in
            ({ model | output = outputText }, Cmd.none)

-- Main view function
view : Model -> Html Msg
view model =
    div []
        [ -- Section for Target Value
              h2 [] [ Html.text "Target Value" ]
            , input [ type_ "text", placeholder "Enter target value", value model.targetValue, onInput TargetChanged ] []

            -- Section for Tolerance Value
            , h2 [] [ Html.text "Tolerance Value (for Approximate mode)" ]
            , input [ type_ "text", placeholder "Enter tolerance value", value model.toleranceValue, onInput ToleranceChanged ] []

            -- Section for Mode Selection
            , h2 [] [ Html.text "Mode" ]
            , button [ onClick ToggleMode ] [ Html.text (case model.mode of
                                                            Exact -> "Exact"
                                                            Approximate -> "Approximate") ]

            -- Section for Input Data with a special note
            , h2 [] [ Html.text "Input Data" ]
            , div []
                [ p [] [ Html.text "Please note:" ]
                , ul []
                    [ li [] [ Html.text "'Dimensionless' (exact spelling) is the only special unit and will vanish under multiplication." ]
                    , li [] [ Html.text "Unparsed lines are interpreted as '0, Dimensionless'." ]
                ]
            , textarea [ onInput InputChanged ] [ Html.text model.inputData ]

            -- Submit button
            , button [ onClick SubmitInput ] [ Html.text "Submit Input" ]

            -- Section for Output
            , h2 [] [ Html.text "Output" ]
            , textarea [ readonly True ] [ Html.text model.output ]
            ]
        ]


extractFloatForTargetBodge : DimExpr -> Float
extractFloatForTargetBodge (x,_) = case x of
    Const y -> y
    _ -> 0.0

parseDimExpr : String -> DimExpr
parseDimExpr line =
    let
        parts = String.split ", " line
    in
    case parts of
        [numExprStr, baseUnitStr] ->
            (parseNumExpr numExprStr, parseBaseUnits baseUnitStr)
        _ -> (Const 0.0, Const "Dimensionless")  -- Default, error handling needed

parseNumExpr : String -> Expr Float
parseNumExpr str =
    -- For simplicity, only handle basic floats for now
    Const (String.toFloat str |> Maybe.withDefault 0.0)

parseBaseUnits : String -> Expr BaseUnits
parseBaseUnits str = Const str

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

main =
    Browser.element 
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view 
        }