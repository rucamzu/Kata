namespace BowlingFs

type Bowl =
    | Bowl of knockedPins:int
    | Bonus

type Frame =
    | Bowling of first:Bowl
    | OpenFrame of first:Bowl * second:Bowl
    | Spare of first:Bowl * second:Bowl * third:Bowl
    | Strike of second:Bowl * third:Bowl

type Game = private Game of frames:Frame list

module Bowl =
    let bowl = Bowl

    let score = function
        | Bowl knockedPins -> knockedPins
        | Bonus -> 0

module Frame =
    let newFrame = function
        | 10 -> Strike (Bonus, Bonus)
        | knockedPins -> knockedPins |> Bowl.bowl |> Bowling

    let bowl knockedPins = function
        | Bowling first when (Bowl.score first) + knockedPins = 10 ->
            Spare (first, Bowl.bowl knockedPins, Bonus)
        | Bowling first ->
            OpenFrame (first, Bowl.bowl knockedPins)
        | Spare (first, second, Bonus) ->
            Spare (first, second, Bowl.bowl knockedPins)
        | Strike (Bonus, Bonus) ->
            Strike (Bowl.bowl knockedPins, Bonus)
        | Strike (second, Bonus) ->
            Strike (second, Bowl.bowl knockedPins)
        | frame -> frame

    let private bowls = function
        | Bowling first -> [first]
        | OpenFrame (first, second) -> [first; second]
        | Spare (first, second, third) -> [first; second; third]
        | Strike (second, third) -> [Bowl.bowl 10; second; third]

    let score = bowls >> List.map Bowl.score >> List.sum

module Game =
    let private ofFrames = Game

    let newGame = ofFrames []

    let private frames = function Game frames' -> frames'

    let bowl knockedPins game =
        match frames game with
        | Bowling _ :: _ ->
            game
            |> frames
            |> List.map (Frame.bowl knockedPins)
            |> ofFrames
        | frames ->
            Frame.newFrame knockedPins :: (List.map (Frame.bowl knockedPins) frames)
            |> ofFrames

    let score = frames >> List.map Frame.score >> List.sum
