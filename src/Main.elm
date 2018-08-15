module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Forms


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { signupForm : Forms.Form
    }


signupFormFields : List ( String, List Forms.FieldValidator )
signupFormFields =
    [ ( "email", emailValidations )
    , ( "password", passwordValidations )
    , ( "age", ageValidations )
    , ( "stooge", stoogeValidations )
    ]


init : ( Model, Cmd Msg )
init =
    { signupForm = Forms.initForm signupFormFields
    }
        ! []



-- Field Validators


emailValidations : List Forms.FieldValidator
emailValidations =
    let
        emailRegex =
            "^\\w+@\\w+\\.\\w+$"
    in
        [ Forms.validateExistence
        , Forms.validateRegex emailRegex
        ]


passwordValidations : List Forms.FieldValidator
passwordValidations =
    [ Forms.validateExistence
    , Forms.validateMinLength 10
    , Forms.validateMaxLength 15
    ]


ageValidations : List Forms.FieldValidator
ageValidations =
    [ Forms.validateExistence
    , Forms.validateNumericality
    , Forms.validateNumericRange 21 88
    ]


stoogeValidations : List Forms.FieldValidator
stoogeValidations =
    [ Forms.validateExistence
    , Forms.validateIsOneOf [ "larry", "curly", "moe" ]
    ]



-- Update


type Msg
    = UpdateFormText String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFormText fieldName value ->
            { model
                | signupForm =
                    Forms.updateFormInput model.signupForm fieldName value
            }
                ! []



-- View


type InputType
    = Email
    | Password
    | Text


formElement : InputType -> String -> Forms.Form -> Html Msg
formElement inputType name form =
    let
        inputId =
            "exampleInput" ++ name

        lowercased =
            String.toLower name

        inputType_ =
            (toString inputType |> String.toLower)
    in
        div [ class "form-group" ]
            [ label [ for inputId ] [ text name ]
            , input
                [ class "form-control"
                , id inputId
                , placeholder ("Enter " ++ lowercased)
                , type_ inputType_
                , onInput (UpdateFormText lowercased)
                ]
                []
            , small [ class "form-text text-muted" ]
                [ text (Forms.errorString form lowercased) ]
            ]


emailFormElement : Forms.Form -> Html Msg
emailFormElement =
    formElement Email "Email"


passwordFormElement : Forms.Form -> Html Msg
passwordFormElement =
    formElement Password "Password"


ageFormElement : Forms.Form -> Html Msg
ageFormElement =
    formElement Text "Age"


stoogeFormElement : Forms.Form -> Html Msg
stoogeFormElement =
    formElement Text "Stooge"


submitButtonAttributes : Bool -> List (Html.Attribute Msg)
submitButtonAttributes validateStatus =
    if validateStatus then
        [ class "btn btn-primary", type_ "submit" ]
    else
        [ class "btn", type_ "submit" ]


signupFormSubmitButton : Forms.Form -> Html Msg
signupFormSubmitButton form =
    button
        (submitButtonAttributes (Forms.validateStatus form))
        [ text "Submit" ]


view : Model -> Html Msg
view model =
    div [ class "container", style [ ( "width", "300px" ) ] ]
        [ Html.form []
            [ emailFormElement model.signupForm
            , passwordFormElement model.signupForm
            , ageFormElement model.signupForm
            , stoogeFormElement model.signupForm
            , signupFormSubmitButton model.signupForm
            ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
