module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Html exposing (..)
import Html.Styled.Events exposing (keyCode, on, onBlur, onCheck, onClick, onInput)
import Http
import HttpBuilder
import Json.Decode as Json exposing (field)
import Json.Encode as Encode
import Task



-- MODEL


type alias Todo =
    { id : Int
    , title : String
    , completed : Bool
    , url : String
    }


todoDecoder : Json.Decoder Todo
todoDecoder =
    Json.map4 Todo
        (field "id" Json.int)
        (field "title" Json.string)
        (field "completed" Json.bool)
        (field "url" Json.string)


todoListDecoder : Json.Decoder (List Todo)
todoListDecoder =
    Json.list todoDecoder


type alias Model =
    { newTodoTitle : String
    , todos : List Todo
    , hideCompleted : Bool
    , todoBeingEdited : Int
    }


type KeyDownWhere
    = OnAdd
    | OnRename


init : () -> ( Model, Cmd Msg )
init _ =
    ( { newTodoTitle = ""
      , todos = []
      , hideCompleted = False
      , todoBeingEdited = -1
      }
    , fetchTodos
    )



-- UPDATE


fetchTodos : Cmd Msg
fetchTodos =
    Http.get
        { url = "https://todo-backend-hanami.herokuapp.com/"
        , expect = Http.expectJson LoadedTodos todoListDecoder
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
        , expect = Http.expectJson AddedTodo todoDecoder
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
        |> HttpBuilder.withExpect (Http.expectJson ChangedCompleted todoDecoder)
        |> HttpBuilder.request


deleteTodo : Todo -> Cmd Msg
deleteTodo todo =
    HttpBuilder.delete todo.url
        |> HttpBuilder.withExpect (Http.expectWhatever DeletedTodo)
        |> HttpBuilder.request


focusTodoEdit : Cmd Msg
focusTodoEdit =
    Task.attempt (\_ -> NoOp) (Dom.focus "todo-edit-input")


addTodo : Model -> ( Model, Cmd Msg )
addTodo model =
    ( { model | newTodoTitle = "" }, postTodo model.newTodoTitle )


type Msg
    = AddTodo
    | ChangeInput String
    | EditTodo Todo String
    | StartEditing Todo
    | EndEditing Todo
    | ChangeCompleted Todo Bool
    | KeyDown KeyDownWhere Int
    | DeleteTodo Todo
    | HideCompleted Bool
    | LoadedTodos (Result Http.Error (List Todo))
    | AddedTodo (Result Http.Error Todo)
    | ChangedCompleted (Result Http.Error Todo)
    | DeletedTodo (Result Http.Error ())
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodo ->
            addTodo model

        ChangeInput title ->
            ( { model | newTodoTitle = title }, Cmd.none )

        ChangeCompleted theTodo checked ->
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

        StartEditing todo ->
            ( { model | todoBeingEdited = todo.id }, focusTodoEdit )

        KeyDown OnAdd keyCode ->
            if keyCode == 13 then
                addTodo model

            else
                ( model, Cmd.none )

        KeyDown OnRename keyCode ->
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

        DeleteTodo theTodo ->
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

        HideCompleted checkedUnchecked ->
            ( { model | hideCompleted = checkedUnchecked }, Cmd.none )

        LoadedTodos (Ok todos) ->
            ( { model | todos = todos }, Cmd.none )

        LoadedTodos (Err _) ->
            ( model, Cmd.none )

        AddedTodo (Ok _) ->
            ( model, fetchTodos )

        AddedTodo (Err _) ->
            ( model, Cmd.none )

        ChangedCompleted (Ok _) ->
            ( model, Cmd.none )

        ChangedCompleted (Err _) ->
            ( model, fetchTodos )

        DeletedTodo (Ok _) ->
            ( model, Cmd.none )

        DeletedTodo (Err _) ->
            ( model, fetchTodos )

        EndEditing theTodo ->
            ( { model
                | todoBeingEdited = -1
              }
            , updateTodo theTodo theTodo.completed theTodo.title
            )

        NoOp ->
            ( model, Cmd.none )

        EditTodo theTodo title ->
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
            , onInput ChangeInput
            , onKeyDown (KeyDown OnAdd)
            , placeholder "What needs to be done"
            ]
            []
        , button [ onClick AddTodo ] [ text "Add" ]
        , div []
            [ input
                [ onCheck HideCompleted
                , type_ "checkbox"
                , Html.checked model.hideCompleted
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
            , if todo.completed then
                textDecoration lineThrough

              else
                textDecoration none
            ]
        ]
        [ div []
            [ input
                [ type_ "checkbox"
                , Html.checked todo.completed
                , onCheck (ChangeCompleted todo)
                ]
                []
            , if model.todoBeingEdited == todo.id then
                input
                    [ id "todo-edit-input"
                    , value todo.title
                    , onBlur (EndEditing todo)
                    , onInput (EditTodo todo)
                    , onKeyDown (KeyDown OnRename)
                    ]
                    []

              else
                label
                    [ onClick (StartEditing todo)
                    ]
                    [ text todo.title ]
            , button
                [ css
                    [ Css.float right
                    ]
                , onClick (DeleteTodo todo)
                ]
                [ text "x" ]
            ]
        ]


viewTodos : Model -> List (Html Msg)
viewTodos model =
    let
        todos =
            if model.hideCompleted == True then
                model.todos
                    |> List.filter (\todo -> todo.completed == False)

            else
                model.todos
    in
    List.map (viewTodo model) todos



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view >> toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }
