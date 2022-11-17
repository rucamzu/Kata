namespace BowlingFs

type Frame =
    | PlayingFrame of first:int
    | OpenFrame of first:int * second:int

type Game = private Game of frames:Frame list

module Frame =
    let score = function
        | PlayingFrame firstBowlScore -> firstBowlScore
        | OpenFrame (firstBowlScore, secondBowlScore) -> firstBowlScore + secondBowlScore

module Game =
    let newGame = Game []

    let private frames = function Game frames' -> frames'

    let score = frames >> List.map Frame.score >> List.sum

    let bowl knockedPins = function
        | Game (PlayingFrame firstBowlScore :: frames) ->
            OpenFrame (firstBowlScore, knockedPins) :: frames
            |> Game
        | Game frames ->
            PlayingFrame knockedPins :: frames
            |> Game