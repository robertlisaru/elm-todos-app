module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes as HSA exposing (..)
import Html.Styled.Events exposing (keyCode, on, onBlur, onCheck, onClick, onInput)
import Http
import HttpBuilder
import Json.Decode as Json exposing (bool, field, int, string)
import Json.Encode as Encode
import Task



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
    , todoBeingEdited : Int
    }


type KeyDownWhere
    = OnAdd
    | OnRename


todoDecoder : Json.Decoder Todo
todoDecoder =
    Json.map4 Todo
        (field "id" Json.int)
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
      , todoBeingEdited = -1
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


updateTodo : Todo -> Bool -> String -> Cmd Msg
updateTodo todo isCompleted title =
    HttpBuilder.patch todo.url
        |> HttpBuilder.withBody
            (Http.jsonBody
                (Encode.object
                    [ ( "completed", Encode.bool isCompleted )
                    , ( "title", Encode.string title )
                    ]
                )
            )
        |> HttpBuilder.withExpect (Http.expectJson CHANGED_COMPLETED todoDecoder)
        |> HttpBuilder.request


deleteTodo : Todo -> Cmd Msg
deleteTodo todo =
    HttpBuilder.delete todo.url
        |> HttpBuilder.withExpect (Http.expectWhatever DELETED_TODO)
        |> HttpBuilder.request


focusTodoEdit : Cmd Msg
focusTodoEdit =
    Task.attempt (\_ -> NoOp) (Dom.focus "todo-edit-input")



-- UPDATE


type Msg
    = ADD_TODO
    | CHANGE_INPUT String
    | EDIT_TODO Todo String
    | START_EDITING Todo
    | END_EDITTING Todo
    | NoOp
    | CHANGE_COMPLETED Todo Bool
    | KEY_DOWN KeyDownWhere Int
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
            , updateTodo theTodo checked theTodo.title
            )

        START_EDITING todo ->
            ( { model | todoBeingEdited = todo.id }, focusTodoEdit )

        KEY_DOWN OnAdd keyCode ->
            if keyCode == 13 then
                addTodo model

            else
                ( model, Cmd.none )

        KEY_DOWN OnRename keyCode ->
            if keyCode == 13 then
                ( { model
                    | todoBeingEdited = -1
                  }
                , let
                    maybeTheTodo =
                        model.todos
                            |> List.filter (\todo -> todo.id == model.todoBeingEdited)
                            |> List.head
                  in
                  case maybeTheTodo of
                    Just theTodo ->
                        updateTodo theTodo theTodo.completed theTodo.title

                    Nothing ->
                        Cmd.none
                )

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

        END_EDITTING theTodo ->
            ( { model
                | todoBeingEdited = -1
              }
            , updateTodo theTodo theTodo.completed theTodo.title
            )

        NoOp ->
            ( model, Cmd.none )

        EDIT_TODO theTodo title ->
            ( { model
                | todos =
                    List.map
                        (\todo ->
                            if todo.id == theTodo.id then
                                { todo | title = title }

                            else
                                todo
                        )
                        model.todos
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ css
            [ maxWidth (px 800)
            , margin auto
            , position relative
            ]
        ]
        [ viewHeader model
        , ul []
            (viewTodos model)
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)


viewHeader : Model -> Html Msg
viewHeader model =
    header
        [ css
            [ textAlign center
            ]
        ]
        [ h1 [] [ text "Todos" ]
        , input
            [ css
                [ Css.width (pct 75)
                ]
            , value model.newTodoTitle
            , onInput CHANGE_INPUT
            , onKeyDown (KEY_DOWN OnAdd)
            , placeholder "What needs to be done"
            ]
            []
        , button [ onClick ADD_TODO ] [ text "Add" ]
        , div []
            [ input
                [ onCheck HIDE_COMPLETED
                , type_ "checkbox"
                , HSA.checked model.hideCompleted
                ]
                []
            , label [] [ text "Hide completed" ]
            ]
        ]


viewTodo : Model -> Todo -> Html Msg
viewTodo model todo =
    li
        [ css
            [ listStyleType none
            , backgroundColor (rgb 240 240 240)
            , padding (px 10)
            , Css.nthChild "even"
                [ backgroundColor (rgb 220 220 220)
                ]
            ]
        ]
        [ div []
            [ input
                [ type_ "checkbox"
                , HSA.checked todo.completed
                , onCheck (CHANGE_COMPLETED todo)
                ]
                []
            , if model.todoBeingEdited == todo.id then
                input
                    [ id "todo-edit-input"
                    , value todo.title
                    , onBlur (END_EDITTING todo)
                    , onInput (EDIT_TODO todo)
                    , onKeyDown (KEY_DOWN OnRename)
                    ]
                    []

              else
                label
                    [ onClick (START_EDITING todo)
                    ]
                    [ text todo.title ]
            , button
                [ css
                    [ Css.float right
                    ]
                , onClick (DELETE_TODO todo)
                ]
                [ text "x" ]
            ]
        ]


viewTodos : Model -> List (Html Msg)
viewTodos model =
    List.map (viewTodo model)
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
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
