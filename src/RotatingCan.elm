module RotatingCan exposing (main)

import Angle exposing (Angle)
import Axis3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cylinder3d exposing (Cylinder3d)
import Direction3d
import Element
import Duration exposing (Duration)
import Html exposing (Html)
import Illuminance
import Length exposing (centimeters, Meters)
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material exposing (Material)
import Scene3d.Mesh as Mesh exposing (Mesh)
import Task
import TriangularMesh
import Vector3d exposing (Vector3d)
import Viewpoint3d
import WebGL.Texture

type WorldCoordinates
    = WorldCoordinates

type Model
    = Loading
        { 
            colorTexture : Maybe (Material.Texture Color) 
        }
    | Loaded
        {
            colorTexture : Material.Texture Color
            , angle : Angle
        }
    | Errored String

type Msg
    = GotColorTexture (Result WebGL.Texture.Error (Material.Texture Color))
    | Tick Duration

init : ( Model, Cmd Msg )
init =
    (Loading
        { colorTexture = Nothing
        }
    , Material.load "http://localhost:8000/textures/goldar.jpg"
        |> Task.attempt GotColorTexture
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    let
        updatedModel =
            case model of
                Loading textures ->
                    case message of
                        GotColorTexture (Ok colorTexture) ->
                            checkIfLoaded { textures | colorTexture = Just colorTexture }
                        GotColorTexture (Err _) ->
                            Errored "Something went wrong loading horrible content"
                        Tick _ ->
                            model
                Loaded loadedModel ->
                    case message of
                        GotColorTexture _ ->
                            model
                        Tick duration ->
                            let
                                rotationRate = 
                                    Angle.degrees 90 |> Quantity.per Duration.second
                                updatedAngle = 
                                    loadedModel.angle |> Quantity.plus (rotationRate |> Quantity.for duration)
                            in
                            Loaded { loadedModel | angle = updatedAngle }
                Errored _ ->
                    model
    in
    ( updatedModel, Cmd.none )


checkIfLoaded :
    { colorTexture : Maybe (Material.Texture Color)
    }
    -> Model
checkIfLoaded textures =
    case ( textures.colorTexture ) of
        ( Just colorTexture ) ->
            Loaded
                { colorTexture = colorTexture
                , angle = Quantity.zero
                }
        _ ->
            Loading textures


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)


sunlight : Light WorldCoordinates Bool
sunlight =
    Light.directional (Light.castsShadows False)
        { chromaticity = Light.sunlight
        , intensity = Illuminance.lux 20000
        , direction = Direction3d.yz (Angle.degrees -120)
        }


sky : Light WorldCoordinates Never
sky = 
    Light.overhead
        { upDirection = Direction3d.positiveZ
        , intensity = Illuminance.lux 7000
        , chromaticity = Light.skylight
        }


environment : Light WorldCoordinates Never
environment =
    Light.overhead
        { upDirection = Direction3d.negativeZ
        , intensity = Illuminance.lux 5000
        , chromaticity = Light.daylight
        }

camera : Camera3d Meters WorldCoordinates
camera =
    Camera3d.orthographic
        { viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = Point3d.centimeters 20 10 5
                , upDirection = Direction3d.positiveZ
                }
            , viewportHeight = Length.centimeters 13
        }


canMesh : Mesh.Bumpy WorldCoordinates
canMesh =
    Mesh.bumpyFaces (tube 0.666)

leftoverCanMesh : Mesh.Bumpy WorldCoordinates
leftoverCanMesh =
    Mesh.bumpyFaces (tube -0.35)

tube fraction =
    TriangularMesh.tube 1 72 <|
        \u v ->
            let
                theta =
                    2 * fraction * pi * v
            in
            { position = Point3d.centimeters (-5 * u) (2 * sin theta) (-2 * cos theta)
            , normal = Vector3d.unsafe { x = cos theta, y = (sin theta), z = 0 }
            , uv = (v, u)
            , tangent = Vector3d.unsafe { x = -(sin theta), y = cos theta, z = 0 }
            , tangentBasisIsRightHanded = True
            }

top : Cylinder3d Meters WorldCoordinates
top =
    let
        start = -5
        end = start + 0.1
    in
    Cylinder3d.along Axis3d.x
        { start = Length.centimeters start
        , end = Length.centimeters end
        , radius = Length.centimeters 1.98
        }

view : Model -> Html msg
view model =
    case model of
        Loaded { colorTexture, angle } ->
            let
                nakedMaterial =
                    Material.metal
                        { baseColor = Color.grey
                        , roughness = 0.5
                        }

                material =
                    Material.texturedMetal
                        { baseColor = colorTexture
                        , roughness = Material.constant 0.5
                        }

                tweakAxis =
                    Axis3d.through Point3d.origin <|
                        Direction3d.positiveY

                rotationAxis =
                    Axis3d.through Point3d.origin <|
                        Direction3d.positiveZ

                
                entities = [Scene3d.cylinder nakedMaterial top , Scene3d.mesh material canMesh, Scene3d.mesh nakedMaterial leftoverCanMesh]
                    |> List.map (Scene3d.rotateAround tweakAxis (Angle.degrees 90))
                    |> List.map (Scene3d.rotateAround rotationAxis angle)

            in
            Element.layout [ Element.width Element.fill, Element.height Element.fill ] <|
                Element.row [ Element.centerX, Element.centerY ]
                    [ Element.html <|
                        Scene3d.custom
                            { camera = camera
                            , clipDepth = centimeters 0.5
                            , dimensions = ( Pixels.int 1000, Pixels.int 1000 )
                            , antialiasing = Scene3d.multisampling
                            , lights = Scene3d.threeLights sunlight sky environment
                            , exposure = Scene3d.exposureValue 11
                            , toneMapping = Scene3d.hableFilmicToneMapping
                            , whiteBalance = Light.daylight
                            , background = Scene3d.transparentBackground
                            , entities = entities
                            }
                    ]
        
        Loading _ ->
            Html.text "Something bean-filled is coming..."
        
        Errored message ->
            Html.text message


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
