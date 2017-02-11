module Main exposing (..)

import Html exposing (..)
import Html.Attributes
import DatePicker
import Date.Extra.Core exposing (intToMonth)
import Date.Extra.Create exposing (dateFromFields)
import Material.Dialog as Dialog
import Material.Button as Button
import Material
import Material.Options
import Material.Typography as Typo
import Material.Options as Options
import Date exposing (Date)
import Date.Extra.Format as DateFormat
import Date.Extra.Config.Config_en_au exposing (config)


type alias Model =
    { datePickerModel : DatePicker.Model
    , date : Date
    , mdl : Material.Model
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
      , date = (dateFromFields 2017 (intToMonth 2) 8 0 0 0 0)
      , mdl = Material.model
      }
    , Cmd.none
    )


datePickerModel : DatePicker.Model
datePickerModel =
    DatePicker.init (dateFromFields 2017 (intToMonth 2) 8 0 0 0 0) "#00bcd4"


view : Model -> Html.Html Msg
view model =
    div [ Html.Attributes.style [ ( "padding", "20px" ) ] ]
        [ Options.styled p
            [ Typo.display3 ]
            [ text <| (DateFormat.format config "%Y-%m-%d") model.date ]
        , (dialogView model)
        , Button.render Mdl
            [ 1 ]
            model.mdl
            [ Dialog.openOn "click" ]
            [ text "Change Date" ]
        , div []
            [ a [ Html.Attributes.href "https://github.com/Leonti/elm-material-datepicker" ] [ text "Project repo" ]
            ]
        ]


dialogView : Model -> Html Msg
dialogView model =
    Dialog.view
        [ Material.Options.css "padding" "0", Material.Options.css "width" "310px" ]
        [ Dialog.content [ Material.Options.css "padding" "0" ]
            [ Html.map DatePickerMsg (DatePicker.view model.datePickerModel)
            ]
        , Dialog.actions []
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Dialog.closeOn "click"
                , Options.onClick DateSelected
                ]
                [ text "Ok" ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Dialog.closeOn "click" ]
                [ text "Cancel" ]
            ]
        ]


type Msg
    = DatePickerMsg DatePicker.Msg
    | Mdl (Material.Msg Msg)
    | DateSelected


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DatePickerMsg message ->
            ( { model
                | datePickerModel = DatePicker.update message model.datePickerModel
              }
            , Cmd.none
            )

        DateSelected ->
            ( { model
                | date = DatePicker.selectedDate model.datePickerModel
              }
            , Cmd.none
            )

        Mdl message ->
            Material.update Mdl message model
