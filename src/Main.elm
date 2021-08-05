module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput)
import Json.Decode as Json



-- MODEL


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    }


type alias Model =
    { newTodoTitle : String
    , idIncrement : Int
    , todos : List Todo
    , hideCompleted : Bool
    }


initialModel : Model
initialModel =
    { newTodoTitle = ""
    , idIncrement = 3
    , todos =
        [ { id = 1
          , title = "Buy milk"
          , completed = True
          }
        , { id = 2
          , title = "Wash dishes"
          , completed = False
          }
        ]
    , hideCompleted = False
    }



-- UPDATE


type Msg
    = ADD_TODO
    | CHANGE_INPUT String
    | CHANGE_COMPLETED Int Bool
    | KEY_DOWN Int
    | DELETE_TODO Int
    | HIDE_COMPLETED Bool


addTodo : Model -> Model
addTodo model =
    { model
        | todos =
            { id = model.idIncrement
            , title = model.newTodoTitle
            , completed = False
            }
                :: model.todos
        , newTodoTitle = ""
        , idIncrement = model.idIncrement + 1
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ADD_TODO ->
            addTodo model

        CHANGE_INPUT title ->
            { model | newTodoTitle = title }

        CHANGE_COMPLETED id checked ->
            { model
                | todos =
                    List.map
                        (\todo ->
                            if todo.id == id then
                                { todo | completed = checked }

                            else
                                todo
                        )
                        model.todos
            }

        KEY_DOWN keyCode ->
            if keyCode == 13 then
                addTodo model

            else
                model

        DELETE_TODO id ->
            { model
                | todos =
                    List.filter
                        (\todo ->
                            todo.id /= id
                        )
                        model.todos
            }

        HIDE_COMPLETED checkedUnchecked ->
            { model | hideCompleted = checkedUnchecked }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model
        , ul []
            (viewTodos model)
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ h1 [] [ text "Todos" ]
        , input
            [ value model.newTodoTitle
            , onInput CHANGE_INPUT
            , onKeyDown KEY_DOWN
            , placeholder "What needs to be done"
            ]
            []
        , button [ onClick ADD_TODO ] [ text "Add" ]
        , div []
            [ input
                [ onCheck HIDE_COMPLETED
                , type_ "checkbox"
                , checked model.hideCompleted
                ]
                []
            , label [] [ text "Hide completed" ]
            ]
        ]


viewTodo : Todo -> Html Msg
viewTodo todo =
    li []
        [ div []
            [ input
                [ type_ "checkbox"
                , checked todo.completed
                , onCheck (CHANGE_COMPLETED todo.id)
                ]
                []
            , label [] [ text todo.title ]
            , button [ onClick (DELETE_TODO todo.id) ] [ text "x" ]
            ]
        ]


viewTodos : Model -> List (Html Msg)
viewTodos model =
    List.map viewTodo
        (if model.hideCompleted == True then
            model.todos
                |> List.filter (\todo -> todo.completed == False)

         else
            model.todos
        )



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
