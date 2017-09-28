module CardGame exposing (..)

import Html exposing (Html, div, text, node)
import Html.Attributes exposing (class, classList, rel, href)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import Debug exposing (log)
import List


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { ids : List String
    , cards : List Card
    , time : Maybe Time
    , gameStatus : GameStatus
    }


type alias Card =
    { id : Int
    , value : String
    , status : CardStatus
    }


type GameStatus
    = Paused
    | Playing
    | Over


type CardStatus
    = Hidden
    | Flipped
    | Matched


init : ( Model, Cmd Msg )
init =
    let
        ids =
            [ "a", "b", "c", "d", "e", "f" ]

        playingCards =
            List.append ids ids
                |> List.indexedMap (\k v -> Card k v Hidden)

        -- TODO: shuffle cards
        shuffledCards =
            playingCards

        initialModel =
            Model ids shuffledCards Nothing Paused
    in
        ( initialModel, Cmd.none )



-- VIEW


css : String -> Html Msg
css path =
    node "link" [ rel "stylesheet", href path ] []


displayCard : Card -> Html Msg
displayCard card =
    div
        [ classList
            [ ( "card", True )
            , ( "hover", card.status /= Hidden )
            ]
        , onClick <| FlipCard card
        ]
        [ div [ class "flipper" ]
            [ div [ class "front" ] [ text "?" ]
            , div [ class "back" ] [ text card.value ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ css "styles.css"
        , div [ class "app" ] (List.map displayCard model.cards)
        ]



-- UPDATE


type Msg
    = Tick Time
    | FlipCard Card


getCardsByStatus : CardStatus -> List Card -> List Card
getCardsByStatus cardStatus cards =
    List.filter (\card -> card.status == cardStatus) cards


doCardsMatch : List Card -> Bool
doCardsMatch cards =
    let
        firstCard =
            List.head cards
                |> Maybe.andThen (\c -> (Just c.value))
                |> Maybe.withDefault "aa"

        secondCard =
            List.drop 1 cards
                |> List.head
                |> Maybe.andThen (\c -> (Just c.value))
                |> Maybe.withDefault "zz"
    in
        firstCard == secondCard


updateCardStatus : Card -> Card -> CardStatus
updateCardStatus currentCard card =
    if currentCard.id == card.id then
        Flipped
    else
        card.status


matchCard : Card -> Card
matchCard card =
    if card.status == Flipped then
        { card | status = Matched }
    else
        card


hideCard : Card -> Card
hideCard card =
    if card.status == Flipped then
        { card | status = Hidden }
    else
        card


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = Just newTime }, Cmd.none )

        FlipCard currentCard ->
            let
                updatedCards =
                    List.map (\card -> { card | status = updateCardStatus currentCard card }) model.cards

                flippedCards =
                    getCardsByStatus Flipped updatedCards

                updatedMatches =
                    if doCardsMatch flippedCards then
                        List.map matchCard updatedCards
                    else if List.length flippedCards > 2 then
                        List.map hideCard updatedCards
                    else
                        updatedCards
            in
                ( { model | cards = updatedMatches }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameStatus == Playing then
        Time.every second Tick
    else
        Sub.none
