module CardGame exposing (..)

import Html exposing (Html, div, text, node, a)
import Html.Attributes exposing (class, classList, rel, href, attribute)
import Html.Events exposing (onClick)
import Random exposing (generate)
import Random.List exposing (shuffle)
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
        , attribute "card-id" <| toString card.id
        ]
        []


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



-- UPDATE


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


type Msg
    = FlipCard Card
    | ShuffleList (List Card)
    | ResetGame
    | NoMatch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShuffleList shuffledList ->
            ( { model | cards = shuffledList }, Cmd.none )

        FlipCard currentCard ->
            let
                updatedCards =
                    List.map (\card -> { card | status = updateCardStatus currentCard card }) model.cards

                flippedCards =
                    getCardsByStatus Flipped updatedCards

                playingCards =
                    if doCardsMatch flippedCards then
                        List.map matchCard updatedCards
                    else
                        updatedCards

                matchedCards =
                    getCardsByStatus Matched playingCards

                gameStatus =
                    if List.length matchedCards == List.length model.cards then
                        Over
                    else
                        Playing
            in
                ( { model | cards = playingCards, gameStatus = gameStatus }, Cmd.none )

        ResetGame ->
            ( initialModel, Cmd.none )

        NoMatch ->
            let
                flippedCards =
                    getCardsByStatus Flipped model.cards

                updatedCards =
                    if List.length flippedCards == 2 then
                        List.map hideCard model.cards
                    else
                        model.cards
            in
                ( { model | cards = updatedCards }, Cmd.none )
