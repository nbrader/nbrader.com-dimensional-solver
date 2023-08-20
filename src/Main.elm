module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra exposing (..)

import Helpers exposing (..)

type BaseUnits = Dimensionless | TimeHours | MoneyPounds
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
allDimExprs args =
    let
        go : List DimExpr -> List DimExpr
        go list =
            case list of
                [] ->
                    []

                [ arg ] ->
                    [ arg ]

                _ ->
                    let
                        n =
                            List.length list
                    in
                    List.concatMap
                        (\i ->
                            let
                                lResult = headTail (List.take i list)
                                rResult = headTail (List.drop i list)
                            in
                            case (lResult, rResult) of
                                (Just ((lExpr, lUnits), lUnitsTail), Just ((rExpr, rUnits), rUnitsTail)) ->
                                    [ (Add lExpr rExpr, Add lUnits rUnits)
                                    , (Sub lExpr rExpr, Sub lUnits rUnits)
                                    , (Mult lExpr rExpr, Mult lUnits rUnits)
                                    , (Div lExpr rExpr, Div lUnits rUnits)
                                    ]
                                _ ->
                                    []  -- Handle the case where either lResult or rResult is Nothing
                        )
                        (List.range 1 (n - 1))
    in
    List.concatMap go (List.concatMap permutationsOfDimExprs (subsets args))

-- Bidirectional mapping between DimExpr and Int
dimExprToInt : List DimExpr -> DimExpr -> Int
dimExprToInt dimExprList dimExpr =
    case elemIndex dimExpr dimExprList of
        Just index -> index
        Nothing -> -1  -- This should not happen if the DimExpr is in the list

intToDimExpr : List DimExpr -> Int -> DimExpr
intToDimExpr dimExprList int =
    List.Extra.getAt int dimExprList |> Maybe.withDefault (Const 0.0, Const Dimensionless)

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
showBaseUnits x = case x of
    Dimensionless -> "Dimensionless"
    TimeHours -> "TimeHours"
    MoneyPounds -> "MoneyPounds"

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
                        numers = List.filter (\x -> x /= Dimensionless) (numers1 ++ numers2)
                        denoms = List.filter (\x -> x /= Dimensionless) (denoms1 ++ denoms2)
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
                        numers = List.filter (\x -> x /= Dimensionless) (numers1 ++ denoms2)
                        denoms = List.filter (\x -> x /= Dimensionless) (denoms1 ++ numers2)
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
                    Const Dimensionless

                [x] ->
                    Const x

                x :: xs ->
                    Mult (Const x) (multiply xs)
    in if denom == Const Dimensionless then
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
                                       Just val -> abs (val - target) < tol
                                       Nothing -> False)
        |> normaliseDimExprs
        |> removeSndFailures
        |> printDimExprs

type alias Model =
    { inputData : String
    , performedSteps : String
    , output : String
    }

type Msg
    = InputChanged String
    | SubmitInput

init : () -> (Model, Cmd Msg)
init _ =
    (
        { inputData = "2.0, TimeHours\n1.0, Dimensionless\n1.0, Dimensionless\n1.0, TimeHours\n1.0, TimeHours"
        , performedSteps = ""
        , output = ""
        },
        Cmd.none
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        InputChanged newInput ->
            ({ model | inputData = newInput }, Cmd.none)

        SubmitInput ->
            let
                -- Parsing the input to get DimExpr values
                dimExprs = String.lines model.inputData |> List.map parseDimExpr
                
                -- Create the output
                outputText = case headTail dimExprs of
                    Nothing -> ""
                    Just (target, inputs) ->
                        let
                            -- Calculate all the DimExpr permutations
                            results = printMatchingDimExprs (extractFloatForTargetBodge target) inputs
                        in String.join "\n" results
            in
            ({ model | output = outputText }, Cmd.none)

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
            (parseNumExpr numExprStr, parseBaseUnit baseUnitStr)
        _ -> (Const 0.0, Const Dimensionless)  -- Default, error handling needed

parseNumExpr : String -> Expr Float
parseNumExpr str =
    -- For simplicity, only handle basic floats for now
    Const (String.toFloat str |> Maybe.withDefault 0.0)

parseBaseUnit : String -> Expr BaseUnits
parseBaseUnit str =
    case str of
        "TimeHours" -> Const TimeHours
        "MoneyPounds" -> Const MoneyPounds
        _ -> Const Dimensionless

view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ Html.text "Input Data" ]
        , textarea [ onInput InputChanged ] [ Html.text model.inputData ]
        , button [ onClick SubmitInput ] [ Html.text "Submit Input" ]
        
        , h2 [] [ Html.text "Performed Steps" ]
        , textarea [ readonly True ] [ Html.text model.performedSteps ]
        
        , h2 [] [ Html.text "Output" ]
        , textarea [ readonly True ] [ Html.text model.output ]
        ]

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