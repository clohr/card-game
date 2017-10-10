module CardGame exposing (..)

import Html exposing (Html, Attribute, div, text, node, a)
import Html.Attributes exposing (class, classList, rel, href, attribute)
import Html.Events exposing (onClick, on)
import Random exposing (generate)
import Random.List exposing (shuffle)
import Json.Decode as Decode
import List


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }



-- MODEL


type alias Model =
    { ids : List String
    , cards : List Card
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


initialModel : Model
initialModel =
    let
        ids =
            [ "a", "b", "c", "d", "e", "f" ]

        playingCards =
            List.append ids ids
                |> List.indexedMap (\k v -> Card k v Hidden)
    in
        Model ids playingCards Paused


init : ( Model, Cmd Msg )
init =
    let
        shuffleCmd =
            generate ShuffleList (shuffle initialModel.cards)
    in
        ( initialModel, shuffleCmd )



-- VIEW


displayCard : Card -> Html Msg
displayCard card =
    node "card-component"
        [ classList [ ( "hover", card.status /= Hidden ) ]
        , attribute "back-text" card.value
        , attribute "card" <| toString card
        , onClick <| FlipCard card
        , handleTransitionEnd UpdateCardsAndGameStatus
        ]
        []


handleTransitionEnd : Msg -> Attribute Msg
handleTransitionEnd toMsg =
    Decode.string
        |> Decode.at [ "target", "card" ]
        |> Decode.map (\_ -> toMsg)
        |> on "flip-card-transition-end"


view : Model -> Html Msg
view model =
    div []
        [ div
            [ classList
                [ ( "game-over", True )
                , ( "hidden", model.gameStatus /= Over )
                ]
            ]
            [ div [ class "message" ] [ text "GAME OVER" ]
            , a [ href "#reset", class "reset-link", onClick ResetGame ] [ text "Play Again" ]
            ]
        , div [ class "app" ] (List.map displayCard model.cards)
        ]



-- HELPERS


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


flipCurrentCard : Card -> Card -> Card
flipCurrentCard currentCard card =
    let
        status =
            if currentCard.id == card.id then
                Flipped
            else
                card.status
    in
        { card | status = status }


updateCardStatus : CardStatus -> Card -> Card
updateCardStatus status card =
    if card.status == Flipped then
        { card | status = status }
    else
        card



-- UPDATE


type Msg
    = FlipCard Card
    | ShuffleList (List Card)
    | ResetGame
    | UpdateCardsAndGameStatus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleList shuffledList ->
            ( { model | cards = shuffledList }, Cmd.none )

        FlipCard currentCard ->
            let
                updatedCards =
                    List.map (flipCurrentCard currentCard) model.cards

                flippedCards =
                    getCardsByStatus Flipped updatedCards

                playingCards =
                    if doCardsMatch flippedCards then
                        List.map (updateCardStatus Matched) updatedCards
                    else
                        updatedCards
            in
                ( { model | cards = playingCards }, Cmd.none )

        ResetGame ->
            ( initialModel, Cmd.none )

        UpdateCardsAndGameStatus ->
            let
                flippedCards =
                    getCardsByStatus Flipped model.cards

                updatedCards =
                    if List.length flippedCards == 2 then
                        List.map (updateCardStatus Hidden) model.cards
                    else
                        model.cards

                matchedCards =
                    getCardsByStatus Matched updatedCards

                gameStatus =
                    if List.length matchedCards == List.length model.cards then
                        Over
                    else
                        Playing
            in
                ( { model | cards = updatedCards, gameStatus = gameStatus }, Cmd.none )
