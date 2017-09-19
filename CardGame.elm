module CardGame exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
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
    , selectedCard : Maybe Card
    , totalMatches : Int
    , startTimer : Bool
    }


type alias Card =
    { id : Int
    , value : String
    , flipped : Bool
    , matched : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        ids =
            [ "a", "b", "c", "d", "e", "f" ]

        cards =
            List.append ids ids

        playingCards =
            List.indexedMap (\k v -> Card k v False False) cards

        -- TODO: shuffle cards
        initialModel =
            Model ids playingCards Nothing Nothing 0 False
    in
        ( initialModel, Cmd.none )



-- VIEW


displayCard : Card -> Html Msg
displayCard card =
    div
        [ classList
            [ ( "card", True )
            , ( "hover", card.flipped )
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
    div [ class "app" ] (List.map displayCard model.cards)



-- UPDATE


type Msg
    = Tick Time
    | FlipCard Card


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = Just newTime }, Cmd.none )

        FlipCard currentCard ->
            let
                updatedCards =
                    List.map (\card -> { card | flipped = currentCard.id == card.id }) model.cards
            in
                ( { model | selectedCard = Just currentCard, cards = updatedCards }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.startTimer then
        Time.every second Tick
    else
        Sub.none
