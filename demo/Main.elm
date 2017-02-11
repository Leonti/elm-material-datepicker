module Main exposing (..)

import Html exposing (..)
import DatePicker
import Date.Extra.Core exposing (intToMonth)
import Date.Extra.Create exposing (dateFromFields)


type alias Model =
    { datePickerModel : DatePicker.Model
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.batch []
        }


init : ( Model, Cmd Msg )
init =
    ( { datePickerModel = datePickerModel
      }
    , Cmd.none
    )


datePickerModel : DatePicker.Model
datePickerModel =
    DatePicker.init (dateFromFields 2017 (intToMonth 2) 8 0 0 0 0) "#00bcd4"


view : Model -> Html.Html Msg
view model =
    div []
        [ Html.map DatePickerMsg (DatePicker.view model.datePickerModel)
        ]


type Msg
    = DatePickerMsg DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DatePickerMsg message ->
            ( { model
                | datePickerModel = DatePicker.update message model.datePickerModel
              }
            , Cmd.none
            )
