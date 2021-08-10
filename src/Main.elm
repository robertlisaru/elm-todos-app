module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Html exposing (..)
import Html.Styled.Events exposing (keyCode, on, onBlur, onCheck, onClick, onInput)
import Http
import HttpBuilder exposing (delete, get, patch, post, request, withBody, withExpect)
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
    , todoBeingRenamed : Maybe Int
    }


type KeyDownWhere
    = OnAdd
    | OnRename


init : () -> ( Model, Cmd Msg )
init _ =
    ( { newTodoTitle = ""
      , todos = []
      , hideCompleted = False
      , todoBeingRenamed = Nothing
      }
    , fetchTodos
    )



-- UPDATE


fetchTodos : Cmd Msg
fetchTodos =
    get "https://todo-backend-hanami.herokuapp.com/"
        |> withExpect (Http.expectJson FetchTodosCmdComplete todoListDecoder)
        |> request


postTodo : String -> Cmd Msg
postTodo title =
    post "https://todo-backend-hanami.herokuapp.com/"
        |> withBody
            (Http.jsonBody
                (Encode.object
                    [ ( "title", Encode.string title ) ]
                )
            )
        |> withExpect (Http.expectJson AddTodoCmdComplete todoDecoder)
        |> request


updateTodo : Todo -> Bool -> String -> Cmd Msg
updateTodo todo isCompleted title =
    patch todo.url
        |> withBody
            (Http.jsonBody
                (Encode.object
                    [ ( "completed", Encode.bool isCompleted )
                    , ( "title", Encode.string title )
                    ]
                )
            )
        |> withExpect (Http.expectJson UpdateCmdComplete todoDecoder)
        |> request


deleteTodo : Todo -> Cmd Msg
deleteTodo todo =
    delete todo.url
        |> withExpect (Http.expectWhatever DeleteTodoCmdComplete)
        |> request


focusRenameInput : Cmd Msg
focusRenameInput =
    Task.attempt (\_ -> NoOp) (Dom.focus "todo-rename-input")


addTodo : Model -> ( Model, Cmd Msg )
addTodo model =
    ( { model | newTodoTitle = "" }, postTodo model.newTodoTitle )


type Msg
    = AddTodo
    | AddTodoCmdComplete (Result Http.Error Todo)
    | DeleteTodo Todo
    | DeleteTodoCmdComplete (Result Http.Error ())
    | EndRenaming Todo
    | FetchTodosCmdComplete (Result Http.Error (List Todo))
    | HideCompleted Bool
    | InputNewTodoTitle String
    | KeyDown KeyDownWhere Int
    | NoOp
    | RenameTodo Todo String
    | StartRenaming Todo
    | UpdateChecked Todo Bool
    | UpdateCmdComplete (Result Http.Error Todo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodo ->
            addTodo model

        AddTodoCmdComplete (Ok _) ->
            ( model, fetchTodos )

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

        DeleteTodoCmdComplete (Err _) ->
            ( model, fetchTodos )

        EndRenaming theTodo ->
            ( { model
                | todoBeingRenamed = Nothing
              }
            , updateTodo theTodo theTodo.completed theTodo.title
            )

        FetchTodosCmdComplete (Ok todos) ->
            ( { model | todos = todos }, Cmd.none )

        HideCompleted checkedUnchecked ->
            ( { model | hideCompleted = checkedUnchecked }, Cmd.none )

        InputNewTodoTitle title ->
            ( { model | newTodoTitle = title }, Cmd.none )

        KeyDown OnAdd 13 ->
            addTodo model

        KeyDown OnRename 13 ->
            ( { model
                | todoBeingRenamed = Nothing
              }
            , let
                maybeTheTodo =
                    model.todos
                        |> List.filter (\todo -> Just todo.id == model.todoBeingRenamed)
                        |> List.head
              in
              case maybeTheTodo of
                Just theTodo ->
                    updateTodo theTodo theTodo.completed theTodo.title

                Nothing ->
                    Cmd.none
            )

        RenameTodo theTodo title ->
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

        StartRenaming todo ->
            ( { model | todoBeingRenamed = Just todo.id }, focusRenameInput )

        UpdateChecked theTodo checked ->
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

        UpdateCmdComplete (Err _) ->
            ( model, fetchTodos )

        _ ->
            ( model, Cmd.none )



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
            [ textAlign center ]
        ]
        [ h1 [] [ text "Todos" ]
        , input
            [ css
                [ Css.width (pct 75)
                ]
            , value model.newTodoTitle
            , onInput InputNewTodoTitle
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
                [ backgroundColor (rgb 220 220 220) ]
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
                , onCheck (UpdateChecked todo)
                ]
                []
            , if model.todoBeingRenamed == Just todo.id then
                viewTodoRenameInput todo

              else
                viewTodoTitle todo
            , button
                [ css
                    [ Css.float right
                    ]
                , onClick (DeleteTodo todo)
                ]
                [ text "x" ]
            ]
        ]


viewTodoRenameInput : Todo -> Html Msg
viewTodoRenameInput todo =
    input
        [ id "todo-rename-input"
        , value todo.title
        , onBlur (EndRenaming todo)
        , onInput (RenameTodo todo)
        , onKeyDown (KeyDown OnRename)
        ]
        []


viewTodoTitle : Todo -> Html Msg
viewTodoTitle todo =
    label
        [ onClick (StartRenaming todo) ]
        [ text todo.title ]


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
