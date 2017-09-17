module CardGame exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
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
    { cards : List Card
    , time : Maybe Time
    , selectedCard : Maybe Card
    , allMatched : Bool
    , startTimer : Bool
    }


type alias Card =
    { id : String
    , flipped : Bool
    , matched : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        ids =
            [ "a", "b", "c", "d", "e", "f" ]

        cards =
            List.map (\i -> Card i False False) ids

        playingCards =
            List.append cards cards

        -- TODO: shuffle cards
        initialModel =
            Model playingCards Nothing Nothing False False
    in
        ( initialModel, Cmd.none )



-- VIEW


displayCard : Card -> Html Msg
displayCard card =
    div [ class "card" ] [ text card.id ]


view : Model -> Html Msg
view model =
    div [ class "app" ] (List.map displayCard model.cards)



-- UPDATE


type Msg
    = Tick Time
    | FlipCard Card
    | EndGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = Just newTime }, Cmd.none )

        FlipCard currentCard ->
            ( { model | selectedCard = Just currentCard }, Cmd.none )

        EndGame ->
            ( { model | allMatched = True }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.startTimer then
        Time.every second Tick
    else
        Sub.none
