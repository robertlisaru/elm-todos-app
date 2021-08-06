module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput)
import Http
import HttpBuilder
import Json.Decode as Json exposing (bool, field, int, string)
import Json.Encode as Encode



-- MODEL


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    , url : String
    }


type alias Model =
    { newTodoTitle : String
    , todos : List Todo
    , hideCompleted : Bool
    }


todoDecoder : Json.Decoder Todo
todoDecoder =
    Json.map4 Todo
        (field "id" int)
        (field "title" string)
        (field "completed" bool)
        (field "url" string)


todoListDecoder : Json.Decoder (List Todo)
todoListDecoder =
    Json.list todoDecoder


init : () -> ( Model, Cmd Msg )
init _ =
    ( { newTodoTitle = ""
      , todos = []
      , hideCompleted = False
      }
    , fetchTodos
    )


fetchTodos : Cmd Msg
fetchTodos =
    Http.get
        { url = "https://todo-backend-hanami.herokuapp.com/"
        , expect = Http.expectJson LOADED_TODOS todoListDecoder
        }


postTodo : String -> Cmd Msg
postTodo title =
    Http.post
        { url = "https://todo-backend-hanami.herokuapp.com/"
        , body =
            Http.jsonBody
                (Encode.object
                    [ ( "title", Encode.string title ) ]
                )
        , expect = Http.expectJson ADDED_TODO todoDecoder
        }


updateTodo : Todo -> Bool -> Cmd Msg
updateTodo todo isCompleted =
    HttpBuilder.patch todo.url
        |> HttpBuilder.withBody
            (Http.jsonBody
                (Encode.object
                    [ ( "completed", Encode.bool isCompleted ) ]
                )
            )
        |> HttpBuilder.withExpect (Http.expectJson CHANGED_COMPLETED todoDecoder)
        |> HttpBuilder.request


deleteTodo : Todo -> Cmd Msg
deleteTodo todo =
    HttpBuilder.delete todo.url
        |> HttpBuilder.withExpect (Http.expectWhatever DELETED_TODO)
        |> HttpBuilder.request



-- UPDATE


type Msg
    = ADD_TODO
    | CHANGE_INPUT String
    | CHANGE_COMPLETED Todo Bool
    | KEY_DOWN Int
    | DELETE_TODO Todo
    | HIDE_COMPLETED Bool
    | LOADED_TODOS (Result Http.Error (List Todo))
    | ADDED_TODO (Result Http.Error Todo)
    | CHANGED_COMPLETED (Result Http.Error Todo)
    | DELETED_TODO (Result Http.Error ())


addTodo : Model -> ( Model, Cmd Msg )
addTodo model =
    ( { model | newTodoTitle = "" }, postTodo model.newTodoTitle )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ADD_TODO ->
            addTodo model

        CHANGE_INPUT title ->
            ( { model | newTodoTitle = title }, Cmd.none )

        CHANGE_COMPLETED theTodo checked ->
            ( { model
                | todos =
                    List.map
                        (\todo ->
                            if todo.id == theTodo.id then
                                { todo | completed = checked }

                            else
                                todo
                        )
                        model.todos
              }
            , updateTodo theTodo checked
            )

        KEY_DOWN keyCode ->
            if keyCode == 13 then
                addTodo model

            else
                ( model, Cmd.none )

        DELETE_TODO theTodo ->
            ( { model
                | todos =
                    List.filter
                        (\todo ->
                            todo.id /= theTodo.id
                        )
                        model.todos
              }
            , deleteTodo theTodo
            )

        HIDE_COMPLETED checkedUnchecked ->
            ( { model | hideCompleted = checkedUnchecked }, Cmd.none )

        LOADED_TODOS (Ok todos) ->
            ( { model | todos = todos }, Cmd.none )

        LOADED_TODOS (Err _) ->
            ( model, Cmd.none )

        ADDED_TODO (Ok _) ->
            ( model, fetchTodos )

        ADDED_TODO (Err _) ->
            ( model, Cmd.none )

        CHANGED_COMPLETED (Ok _) ->
            ( model, Cmd.none )

        CHANGED_COMPLETED (Err _) ->
            ( model, fetchTodos )

        DELETED_TODO (Ok _) ->
            ( model, Cmd.none )

        DELETED_TODO (Err _) ->
            ( model, fetchTodos )



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
                , onCheck (CHANGE_COMPLETED todo)
                ]
                []
            , label [] [ text todo.title ]
            , button [ onClick (DELETE_TODO todo) ] [ text "x" ]
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
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
