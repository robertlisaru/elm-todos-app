module Main exposing (main)

import Browser
import Html exposing (..)



-- MODEL


initialModel =
    { todos =
        [ { title = "Buy milk" }
        , { title = "Wash dishes" }
        ]
    }



-- UPDATE


update msg model =
    case msg of
        _ ->
            model



-- VIEW


view model =
    ul []
        (viewTodos model.todos)


viewTodo todo =
    li []
        [ h1 [] [ text todo.title ]
        ]


viewTodos todos =
    List.map viewTodo todos



-- MAIN


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
