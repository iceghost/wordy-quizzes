module Main exposing (main)

import Array exposing (Array)
import Browser
import Color.OneDark as OneDark
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Random
import Random.Array
import Random.List
import RemoteData exposing (WebData)


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { hostname : String
    , name : String
    , today : WebData (Array Quiz)
    , learned : WebData (Array Quiz)
    , state : State
    , score : Int
    }


type State
    = EnterName
    | SelectRevision
    | LearnNew (Array Quiz) AnswerResult Int (List ( Bool, String ))
    | Generating
    | QuizResult


type AnswerResult
    = NotYet
    | Correct
    | Incorrect


type alias Quiz =
    { question : String
    , answer : String
    }


parseQuizzes : String -> Array Quiz
parseQuizzes raw =
    raw
        |> String.lines
        |> List.map
            (String.trim
                >> (\line ->
                        let
                            index =
                                String.indexes ":" line
                                    |> List.head
                                    |> Maybe.withDefault 0
                        in
                        Quiz (String.slice 0 index line |> String.trim)
                            (String.slice (index + 1) (String.length line + 1) line |> String.trim)
                   )
            )
        |> Array.fromList


init : String -> ( Model, Cmd Msg )
init hostname =
    ( { hostname = hostname
      , name = ""
      , today = RemoteData.Loading
      , learned = RemoteData.Loading
      , state = EnterName
      , score = 0
      }
    , Http.get
        { url = hostname ++ "/public/today.txt"
        , expect =
            Http.expectString
                (RemoteData.fromResult
                    >> RemoteData.map parseQuizzes
                    >> FetchToday
                )
        }
    )



-- UPDATE


type Msg
    = InputName String
    | SubmitName
    | FetchToday (WebData (Array Quiz))
    | ShuffleToday (WebData (Array Quiz))
    | FetchLearned (WebData (Array Quiz))
    | ShuffleLearned (WebData (Array Quiz))
    | SelectToday
    | SelectLearned
    | GenerateAnswer (Array Quiz) Int
    | GotAnswer (Array Quiz) Int (List ( Bool, String ))
    | SelectAnswer Bool
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.today, model.learned ) of
        ( RemoteData.Success today, RemoteData.Success learned ) ->
            case msg of
                InputName name ->
                    ( { model | name = name }, Cmd.none )

                SubmitName ->
                    ( { model | state = SelectRevision }, Cmd.none )

                SelectToday ->
                    update (GenerateAnswer today 0) { model | score = 0 }

                SelectLearned ->
                    update (GenerateAnswer learned 0) { model | score = 0 }

                GenerateAnswer quizzes currentIndex ->
                    let
                        currentQuiz =
                            Array.get currentIndex quizzes |> Maybe.withDefault (Quiz "" "")

                        chosenAnswers =
                            quizzes
                                |> Array.toList
                                |> List.partition ((==) currentQuiz)
                                |> Tuple.second
                                |> List.map (\quiz -> ( False, quiz.answer ))
                    in
                    if currentIndex < Array.length quizzes then
                        ( { model | state = Generating }
                        , Random.generate (GotAnswer quizzes currentIndex)
                            (Random.List.shuffle chosenAnswers
                                |> Random.andThen
                                    (\answers ->
                                        Random.List.shuffle (( True, currentQuiz.answer ) :: List.take 3 answers)
                                    )
                            )
                        )

                    else
                        ( { model | state = QuizResult }, Cmd.none )

                GotAnswer quizzes currentIndex answer ->
                    let
                        currentQuiz =
                            Array.get currentIndex quizzes |> Maybe.withDefault (Quiz "" "")
                    in
                    ( { model | state = LearnNew quizzes NotYet currentIndex answer }, Cmd.none )

                SelectAnswer result ->
                    case model.state of
                        LearnNew quizzes _ current answers ->
                            if result == True then
                                ( { model | state = LearnNew quizzes Correct current answers, score = model.score + 1 }, Cmd.none )

                            else
                                ( { model | state = LearnNew quizzes Incorrect current answers }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Reset ->
                    ( { model | state = SelectRevision }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        _ ->
            case msg of
                FetchToday data ->
                    ( { model | today = data }
                    , case data of
                        RemoteData.Success quizzes ->
                            Random.generate (RemoteData.Success >> ShuffleToday)
                                (Random.Array.shuffle quizzes)

                        _ ->
                            Cmd.none
                    )

                ShuffleToday data ->
                    ( { model | today = data }
                    , Http.get
                        { url = model.hostname ++ "/public/learned.txt"
                        , expect =
                            Http.expectString
                                (RemoteData.fromResult
                                    >> RemoteData.map parseQuizzes
                                    >> FetchLearned
                                )
                        }
                    )

                FetchLearned data ->
                    ( { model | learned = data }
                    , case data of
                        RemoteData.Success quizzes ->
                            Random.generate (RemoteData.Success >> ShuffleLearned)
                                (Random.Array.shuffle quizzes)

                        _ ->
                            Cmd.none
                    )

                ShuffleLearned data ->
                    ( { model | learned = data }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color OneDark.black
        , Font.color OneDark.blue
        , Font.size 20
        , Font.semiBold
        ]
        (case ( model.today, model.learned ) of
            ( RemoteData.Success today, RemoteData.Success learned ) ->
                case model.state of
                    EnterName ->
                        viewEnterName model.name

                    SelectRevision ->
                        viewSelect

                    Generating ->
                        Element.text "Loading new q..."

                    LearnNew quizzes answerResult index answers ->
                        viewQuiz answerResult quizzes index answers

                    QuizResult ->
                        viewResult model.name model.score

            ( RemoteData.Loading, _ ) ->
                Element.text "Loading..."

            ( _, RemoteData.Loading ) ->
                Element.text "Loading..."

            _ ->
                Element.text "Lỗi tải dữ liệu. Vui lòng tải lại trang..."
        )


viewEnterName : String -> Element Msg
viewEnterName name =
    Element.column
        [ Element.width (Element.maximum 500 Element.fill)
        , Element.centerY
        , Element.centerX
        , Element.spacing 10
        , Font.size 30
        ]
        [ Input.text
            [ Font.color OneDark.gutterGrey ]
            { onChange = InputName
            , text = name
            , placeholder = Nothing
            , label = Input.labelAbove [ Font.center ] (Element.text "Enter your name")
            }
        , Input.button
            [ Background.color OneDark.blue
            , Font.color OneDark.black
            , Border.rounded 5
            , Element.padding 10
            , Element.centerX
            ]
            { onPress = Just SubmitName
            , label = Element.text "Let's goooo"
            }
        ]


viewSelect : Element Msg
viewSelect =
    Element.column
        [ Element.width (Element.maximum 500 Element.fill)
        , Element.centerY
        , Element.centerX
        , Element.spacing 20
        , Font.size 40
        ]
        [ Input.button [] { onPress = Just SelectToday, label = Element.text "Today's words" }
        , Input.button [] { onPress = Just SelectLearned, label = Element.text "Learned words" }
        ]


viewQuiz : AnswerResult -> Array Quiz -> Int -> List ( Bool, String ) -> Element Msg
viewQuiz result quizzes index answers =
    let
        currentQuiz =
            Array.get index quizzes |> Maybe.withDefault (Quiz "" "")
    in
    Element.column
        [ Element.width (Element.maximum 500 Element.fill)
        , Element.centerY
        , Element.centerX
        , Element.spacing 10
        ]
        [ Element.text ("Question " ++ String.fromInt (index + 1) ++ "/" ++ String.fromInt (Array.length quizzes))
        , Element.paragraph [ Font.center, Font.color OneDark.darkYellow, Font.size 40 ] [ Element.text currentQuiz.question ]
        , case result of
            NotYet ->
                Element.column [ Element.width Element.fill ] (List.map viewAnswer answers ++ [ Element.text "" ])

            _ ->
                Element.column
                    [ Element.width Element.fill
                    , Element.below
                        (Input.button [ Element.paddingXY 0 5 ]
                            { onPress = Just (GenerateAnswer quizzes (index + 1))
                            , label = Element.text "Câu kế"
                            }
                        )
                    ]
                    (List.map viewCheckedAnswer answers
                        ++ []
                    )
        ]


viewAnswer : ( Bool, String ) -> Element Msg
viewAnswer ( value, answer ) =
    Input.button
        [ Element.width Element.fill
        , Element.paddingXY 0 10
        ]
        { onPress = Just (SelectAnswer value)
        , label = Element.paragraph [] [ Element.text ("> " ++ answer) ]
        }


viewCheckedAnswer : ( Bool, String ) -> Element Msg
viewCheckedAnswer ( value, answer ) =
    Input.button
        (if value then
            [ Background.color OneDark.green
            , Font.color OneDark.gutterGrey
            , Element.width Element.fill
            , Element.paddingXY 0 10
            ]

         else
            [ Background.color OneDark.darkRed
            , Font.color OneDark.white
            , Element.width Element.fill
            , Element.paddingXY 0 10
            ]
        )
        { onPress = Nothing, label = Element.paragraph [] [ Element.text ("> " ++ answer) ] }


viewResult : String -> Int -> Element Msg
viewResult name score =
    Element.column
        [ Element.centerY
        , Element.centerX
        , Font.size 50
        ]
        [ Element.paragraph []
            [ Element.text
                (if name == "" then
                    "Your"

                 else
                    name ++ "'s"
                )
            , Element.text " score is "
            , Element.el [ Font.color OneDark.green ] (Element.text (String.fromInt score ++ "/10"))
            ]
        , Element.text
            (if score == 10 then
                "Ok cool \u{1F644}"

             else if score == 9 then
                "So close! You can do it! 😍"

             else if score == 8 then
                "Great! Let's do better next time 😎"

             else if score == 7 then
                "Nice! But is that all you got? 😗"

             else
                "Please try again... 🙂"
            )
        , Input.button [ Font.underline, Font.color OneDark.darkRed ] { onPress = Just Reset, label = Element.text "Reset" }
        ]
