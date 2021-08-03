module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)



-- MODEL


type alias Todo =
    { title : String
    , completed : Bool
    }


type alias Model =
    { todos : List Todo }


initialModel : Model
initialModel =
    { todos =
        [ { title = "Buy milk"
          , completed = True
          }
        , { title = "Wash dishes"
          , completed = False
          }
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
    div []
        [ viewHeader
        , ul []
            (viewTodos model.todos)
        ]


viewHeader : Html msg
viewHeader =
    header []
        [ h1 [] [ text "Todos" ]
        , input [ placeholder "What needs to be done" ] []
        , button [] [ text "Add" ]
        ]


viewTodo : Todo -> Html msg
viewTodo todo =
    li []
        [ div []
            [ input
                [ type_ "checkbox"
                , checked todo.completed
                ]
                []
            , span [] [ text todo.title ]
            ]
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
