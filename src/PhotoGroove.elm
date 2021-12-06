module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo, selectedPhotoURL : String, chosenSize : ThumbnailSize }


type Msg
    = ClickedPhoto String
    | ClickedSizeRadioButton ThumbnailSize
    | ClickedSupriseMeButton
    | GotRandomPhotoIndex Int


initialModel : Model
initialModel =
    { photos = [ { url = "1.jpeg" }, { url = "2.jpeg" }, { url = "3.jpeg" } ]
    , selectedPhotoURL = "1.jpeg"
    , chosenSize = Medium
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewRadioButton : ThumbnailSize -> Html Msg
viewRadioButton size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSizeRadioButton size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedPhotoURL photo =
    img [ src (urlPrefix ++ photo.url), classList [ ( "selected", photo.url == selectedPhotoURL ) ], onClick (ClickedPhoto photo.url) ] []


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Photo Groove" ]
        , button [ onClick ClickedSupriseMeButton ] [ text "Surprise Me!" ]
        , div [ id "choose-size" ] (List.map viewRadioButton [ Small, Medium, Large ])
        , div [ id "thumbnails", class (sizeToString model.chosenSize) ] (List.map (viewThumbnail model.selectedPhotoURL) model.photos)
        , img [ src (urlPrefix ++ "large/" ++ model.selectedPhotoURL) ] []
        ]


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


getPhotoAtIndex : Int -> String
getPhotoAtIndex index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto photo ->
            ( { model | selectedPhotoURL = photo }, Cmd.none )

        ClickedSizeRadioButton newSize ->
            ( { model | chosenSize = newSize }, Cmd.none )

        ClickedSupriseMeButton ->
            ( model, Random.generate GotRandomPhotoIndex randomPhotoPicker )

        GotRandomPhotoIndex index ->
            ( { model | selectedPhotoURL = getPhotoAtIndex index }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
