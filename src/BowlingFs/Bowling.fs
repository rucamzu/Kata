namespace BowlingFs

type Bowl = private Bowl of knockedPins:int

type Frame =
    | Bowling of first:Bowl
    | OpenFrame of first:Bowl * second:Bowl

type Game = private Game of frames:Frame list

module Bowl =
    let make = Bowl

    let score = function Bowl knockedPins -> knockedPins

module Frame =
    let newFrame = Bowl.make >> Bowling

    let bowl knockedPins = function
        | Bowling first -> OpenFrame (first, Bowl.make knockedPins)
        | frame -> frame

    let score = function
        | Bowling first -> Bowl.score first
        | OpenFrame (first, second) ->
            [first; second]
            |> List.map Bowl.score
            |> List.sum

module Game =
    let newGame = Game []

    let private frames = function Game frames' -> frames'

    let score = frames >> List.map Frame.score >> List.sum

    let bowl knockedPins = function
        | Game (Bowling first :: frames) ->
            Frame.bowl knockedPins (Bowling first) :: frames
            |> Game
        | Game frames ->
            Frame.newFrame knockedPins :: frames
            |> Game