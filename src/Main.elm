module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MODEL


type alias Todo =
    { title : String
    , completed : Bool
    }


type alias Model =
    { newTodoTitle : String
    , todos : List Todo
    }


initialModel : Model
initialModel =
    { newTodoTitle = ""
    , todos =
        [ { title = "Buy milk"
          , completed = True
          }
        , { title = "Wash dishes"
          , completed = False
          }
        ]
    }



-- UPDATE


type Msg
    = ADD_TODO
    | CHANGE_INPUT String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ADD_TODO ->
            { model
                | todos = { title = model.newTodoTitle, completed = False } :: model.todos
                , newTodoTitle = ""
            }

        CHANGE_INPUT title ->
            { model | newTodoTitle = title }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , ul []
            (viewTodos model.todos)
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ h1 [] [ text "Todos" ]
        , input
            [ value model.newTodoTitle
            , onInput CHANGE_INPUT
            , placeholder "What needs to be done"
            ]
            []
        , button [ onClick ADD_TODO ] [ text "Add" ]
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


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
