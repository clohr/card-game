module CardGame exposing (..)

import Html exposing (Html, div, text, node)
import Html.Attributes exposing (class, classList, rel, href)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = Just newTime }, Cmd.none )

        FlipCard currentCard ->
            let
                setCardStatus : Card -> Model -> CardStatus
                setCardStatus card model =
                    if currentCard.id == card.id then
                        Flipped
                    else
                        card.status

                updatedCards =
                    List.map (\card -> { card | status = setCardStatus card model }) model.cards
            in
                ( { model | cards = updatedCards }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameStatus == Playing then
        Time.every second Tick
    else
        Sub.none
