namespace BowlingFs

type Bowl =
    | Bowl of knockedPins:int
    | Bonus

type Frame =
    | Bowling of first:Bowl
    | OpenFrame of first:Bowl * second:Bowl
    | Spare of first:Bowl * second:Bowl * third:Bowl

type Game = private Game of frames:Frame list

module Bowl =
    let bowl = Bowl

    let score = function
        | Bowl knockedPins -> knockedPins
        | Bonus -> 0

module Frame =
    let newFrame = Bowl.bowl >> Bowling

    let bowl knockedPins = function
        | Bowling first ->
            match (Bowl.score first) + knockedPins with
            | 10 -> Spare (first, Bowl.bowl knockedPins, Bonus)
            | _ -> OpenFrame (first, Bowl.bowl knockedPins)
        | Spare (first, second, Bonus) ->
            Spare (first, second, Bowl.bowl knockedPins)
        | frame -> frame

    let private bowls = function
        | Bowling first -> [first]
        | OpenFrame (first, second) -> [first; second]
        | Spare (first, second, third) -> [first; second; third]

    let score = bowls >> List.map Bowl.score >> List.sum

module Game =
    let newGame = Game []

    let bowl knockedPins = function
        | Game (Bowling first :: frames) ->
            Frame.bowl knockedPins (Bowling first) :: frames
            |> Game
        | Game (Spare (first, second, Bonus) :: frames) ->
            Frame.newFrame knockedPins :: (Frame.bowl knockedPins (Spare (first, second, Bonus)) :: frames)
            |> Game
        | Game frames ->
            Frame.newFrame knockedPins :: frames
            |> Game

    let private frames = function Game frames' -> frames'

    let score = frames >> List.map Frame.score >> List.sum
