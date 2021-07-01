module PhotoGroove exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (class, classList, id, name, src, title, type_, checked)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random

urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"  

type ThumbnailSize
    = Small
    | Medium
    | Large

type Msg   
    = ClickedPhoto String   
    | ClickedSize ThumbnailSize  
    | ClickedSurpriseMe  
    | GotRandomPhoto Photo  
    | GotPhotos (Result Http.Error (List Photo))

type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String

type alias Photo =
    { url : String 
    , size : Int
    , title : String
    }

type alias Model = 
   { status : Status
   , chosenSize : ThumbnailSize
   }

viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =  
    img      
        [ src (urlPrefix ++ thumb.url)    
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")      
        , classList [ ( "selected", selectedUrl == thumb.url ) ]   
        , onClick (ClickedPhoto thumb.url)        ]        
        []

viewSizeChooser : ThumbnailSize -> Bool -> Html Msg
viewSizeChooser size isChecked = 
    label []
        [ input [ type_ "radio"
                , name "size"
                , onClick (ClickedSize size)
                , checked isChecked
                ] []
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

view : Model -> Html Msg
view model = 
    div [ class "content" ] <|
    case model.status of
        Loaded photos selectedUrl ->
            viewLoaded photos selectedUrl model.chosenSize
        Loading ->
            []
        Errored errorMessage ->
            [ text ("Error " ++ errorMessage ) ]

viewFilter : String -> Int -> Html Msg
viewFilter name magnitude =   
    div [ class "filter-slider" ]
        [ label [] [ text name ]         
        , rangeSlider                                                     
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)           
            ] 
            []     
        , label [] [ text (String.fromInt magnitude) ]         
        ]
    

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
            [ h1 [] [ text "Photo Groove" ]
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , div [ class "filters" ]
        [ viewFilter "Hue" 0 
        , viewFilter "Ripple" 0
        , viewFilter "Noise" 0
        ]
        , h3 [] [ text "Thumbnail Size:" ]
        , div [ id "choose-size" ]
            [ viewSizeChooser Small (chosenSize == Small)
            , viewSizeChooser Medium (chosenSize == Medium)
            , viewSizeChooser Large (chosenSize == Large)
            ]
        , div [ id "thumbnails", class (sizeToString chosenSize) ]
            (List.map
                (viewThumbnail selectedUrl) photos )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ selectedUrl)
            ]
            []
        ]

initialModel : Model
initialModel =    
    { status = Loading
    , chosenSize = Medium
    }

initialCmd : Cmd Msg
initialCmd = 
    Http.get      
        { url = "http://elm-in-action.com/photos/list.json"    
        , expect = Http.expectJson GotPhotos (list photoDecoder)   
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =  
    case msg of
        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )
        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )
        ClickedSurpriseMe ->   
            case model.status of
                    Loaded (firstPhoto :: otherPhotos) _ ->
                        Random.uniform firstPhoto otherPhotos 
                        |> Random.generate GotRandomPhoto  
                        |> Tuple.pair model
                    Loading ->
                        ( model, Cmd.none )  
                    Loaded [] _ ->
                        ( model, Cmd.none )
                    Errored errorMessage ->   
                        ( model, Cmd.none )
        GotPhotos (Ok photos) ->
            case photos of             
                first :: rest ->   
                    ( { model | status = Loaded photos first.url }    
                    , Cmd.none     
                    )   

                [] ->             
                    ( { model | status = Errored "0 photos found" }, Cmd.none )   
        GotPhotos (Err httpError) ->    
                ( { model | status = Errored "Server error!" }, Cmd.none )

selectUrl : String -> Status -> Status
selectUrl url status = 
    case status of
        Loaded photos _ ->
            Loaded photos url
        Loading ->
            status
        Errored errorMessage ->
            status

photoDecoder : Decoder Photo
photoDecoder =  
    succeed Photo    
        |> required "url" string   
        |> required "size" int                     
        |> optional "title" string "(untitled)"

rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =  
    node "range-slider" attributes children

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }