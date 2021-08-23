module RotatingCan exposing (main)

import Angle exposing (Angle)
import Axis3d
import Browser
import Browser.Events
import Camera3d exposing (Camera3d)
import Color exposing (Color)
import Cylinder3d exposing (Cylinder3d)
import Direction3d
import Duration exposing (Duration)
import Element
import Html exposing (Html)
import Illuminance
import Length exposing (Meters, centimeters)
import List
import Pixels
import Point3d
import Quantity
import Scene3d
import Scene3d.Light as Light exposing (Light)
import Scene3d.Material as Material
import Scene3d.Mesh as Mesh
import Task
import TriangularMesh
import Vector3d
import Viewpoint3d
import WebGL.Texture


type WorldCoordinates
    = WorldCoordinates


type Model
    = Loading
        { colorTexture : Maybe (Material.Texture Color)
        }
    | Loaded
        { colorTexture : Material.Texture Color
        , labelSide : Scene3d.Entity WorldCoordinates
        , metalSide : Scene3d.Entity WorldCoordinates
        , top : Scene3d.Entity WorldCoordinates
        , bottom : Scene3d.Entity WorldCoordinates
        , angle : Angle
        }
    | Errored String


type Msg
    = GotColorTexture (Result WebGL.Texture.Error (Material.Texture Color))
    | Tick Duration


init : ( Model, Cmd Msg )
init =
    ( Loading
        { colorTexture = Nothing
        }
    , Material.load "https://launi.me/dirigim.us/textures/goldar.jpg"
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
                                    Angle.degrees 7 |> Quantity.per Duration.second

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
    case textures.colorTexture of
        Just colorTexture ->
            let
                labelTexture =
                    Material.texturedMatte colorTexture

                nakedMaterial =
                    Material.metal
                        { baseColor = Color.grey
                        , roughness = 0.1
                        }
            in
            Loaded
                { colorTexture = colorTexture
                , labelSide = makeShadowScene labelTexture canMesh
                , metalSide = makeShadowScene labelTexture leftoverCanMesh
                , top = Scene3d.cylinder nakedMaterial (canEnd -5.1)
                , bottom = Scene3d.cylinder nakedMaterial (canEnd 0)
                , angle = Quantity.zero
                }

        _ ->
            Loading textures


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (Duration.milliseconds >> Tick)


sunlight : Light WorldCoordinates Bool
sunlight =
    Light.directional (Light.castsShadows False)
        { chromaticity = Light.incandescent
        , intensity = Illuminance.lux 100000
        , direction = Direction3d.xyZ (Angle.degrees -120) (Angle.degrees 120)
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
    Camera3d.perspective
        { viewpoint =
            Viewpoint3d.lookAt
                { focalPoint = Point3d.origin
                , eyePoint = Point3d.centimeters 0 19.9 6
                , upDirection = Direction3d.positiveZ
                }
        , verticalFieldOfView = Angle.degrees 29
        }


canMesh : Mesh.Textured WorldCoordinates
canMesh =
    Mesh.texturedFaces (tube 0.666)


leftoverCanMesh : Mesh.Textured WorldCoordinates
leftoverCanMesh =
    Mesh.texturedFaces (tube -0.35)


tube fraction =
    TriangularMesh.tube 1 72 <|
        \u v ->
            let
                theta =
                    2 * fraction * pi * v
            in
            { position = Point3d.centimeters (-5 * u) (2 * sin theta) (-2 * cos theta)
            , normal = Vector3d.unsafe { x = cos theta, y = sin theta, z = 0 }
            , uv = ( v, u )
            }


canEnd : Float -> Cylinder3d Meters WorldCoordinates
canEnd start =
    let
        end =
            start + 0.1
    in
    Cylinder3d.along Axis3d.x
        { start = Length.centimeters start
        , end = Length.centimeters end
        , radius = Length.centimeters 2
        }


makeShadowScene material mesh =
    Scene3d.meshWithShadow material mesh (Mesh.shadow mesh)


view : Model -> Html msg
view model =
    case model of
        Loaded { labelSide, metalSide, top, bottom, angle } ->
            let
                tweakAxis =
                    Axis3d.through Point3d.origin <|
                        Direction3d.positiveY

                rotationAxis =
                    Axis3d.through Point3d.origin <|
                        Direction3d.positiveZ

                entities =
                    [ top
                    , bottom
                    , labelSide
                    , metalSide
                    ]
                        |> List.map (Scene3d.rotateAround tweakAxis (Angle.degrees 90))
                        |> List.map (Scene3d.rotateAround rotationAxis angle)
            in
            Element.layout [ Element.width Element.fill, Element.height Element.fill ] <|
                Element.row [ Element.centerX, Element.centerY ]
                    [ Element.html <|
                        Scene3d.custom
                            { camera = camera
                            , clipDepth = centimeters 0.1
                            , dimensions = ( Pixels.int 500, Pixels.int 500 )
                            , antialiasing = Scene3d.multisampling
                            , lights = Scene3d.threeLights sunlight sky environment
                            , exposure = Scene3d.exposureValue 10.5
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
