module Main exposing (main)

import Browser
import Html exposing (..)



-- MODEL


type alias Todo =
    { title : String }


type alias Model =
    { todos : List Todo }


initialModel : Model
initialModel =
    { todos =
        [ { title = "Buy milk" }
        , { title = "Wash dishes" }
        ]
    }



-- UPDATE


update : msg -> Model -> Model
update msg model =
    case msg of
        _ ->
            model



-- VIEW


view : Model -> Html msg
view model =
    ul []
        (viewTodos model.todos)


viewTodo : Todo -> Html msg
viewTodo todo =
    li []
        [ h1 [] [ text todo.title ]
        ]


viewTodos : List Todo -> List (Html msg)
viewTodos todos =
    List.map viewTodo todos



-- MAIN


main : Program () Model msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
