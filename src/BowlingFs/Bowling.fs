namespace BowlingFs

type Bowl =
    | Bowl of knockedPins:int
    | Bonus

type Frame =
    | Bowling of first:Bowl
    | OpenFrame of first:Bowl * second:Bowl
    | Spare of first:Bowl * second:Bowl * third:Bowl
    | Strike of second:Bowl * third:Bowl

type Game =
    | Game of frames:Frame list
    | FinishedGame of frames:Frame list

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

    let (|WithBowlsRemaining|_|) frame =
        match frame with
        | Bowling _ -> Some frame
        | _ -> None

module Game =
    let private ofFrames frames =
        match frames with
        | Frame.WithBowlsRemaining _ :: _ -> Game frames
        | frames' when frames'.Length < 10 -> Game frames
        | _ -> FinishedGame frames

    let newGame = ofFrames []

    let private frames = function
        | Game frames' -> frames'
        | FinishedGame frames' -> frames'

    let bowl knockedPins game =
        match game with
        | Game (Bowling _ :: _) ->
            game
            |> frames
            |> List.map (Frame.bowl knockedPins)
            |> ofFrames
        | Game frames when frames.Length < 10 ->
            Frame.newFrame knockedPins :: (List.map (Frame.bowl knockedPins) frames)
            |> ofFrames
        | Game frames ->
            frames
            |> List.map (Frame.bowl knockedPins)
            |> ofFrames
        | _ -> game


    let score = frames >> List.map Frame.score >> List.sum

    let finished = function
        | FinishedGame _ -> true
        | _ -> false