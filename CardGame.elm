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
    , selectedCards : List Card
    , totalMatches : Int
    , gameStatus : GameStatus
    }


type GameStatus
    = Paused
    | Playing
    | Over


type CardStatus
    = Hidden
    | Flipped
    | Matched


type alias Card =
    { id : Int
    , value : String
    , status : CardStatus
    }


init : ( Model, Cmd Msg )
init =
    let
        ids =
            [ "a", "b", "c", "d", "e", "f" ]

        cards =
            List.append ids ids

        playingCards =
            List.indexedMap (\k v -> Card k v Hidden) cards

        -- TODO: shuffle cards
        initialModel =
            Model ids playingCards Nothing [] 0 Paused
    in
        ( initialModel, Cmd.none )



-- VIEW


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
                checkIfSelected : Card -> Model -> CardStatus
                checkIfSelected card model =
                    if List.member card model.selectedCards then
                        Flipped
                    else
                        Hidden

                setCardStatus : Card -> Model -> CardStatus
                setCardStatus card model =
                    if currentCard.id == card.id then
                        Flipped
                    else
                        checkIfSelected card model

                setSelected : Card -> Model -> List Card
                setSelected card model =
                    -- only want to compare two cards at a time
                    if List.length model.selectedCards < 2 then
                        { card | status = Flipped } :: model.selectedCards
                    else
                        model.selectedCards

                updatedCards =
                    if List.length model.selectedCards < 2 then
                        List.map (\card -> { card | status = setCardStatus card model }) model.cards
                    else
                        model.cards
            in
                ( { model
                    | selectedCards = setSelected currentCard model
                    , cards = updatedCards
                  }
                , Cmd.none
                )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameStatus == Playing then
        Time.every second Tick
    else
        Sub.none
