namespace BowlingFs

type Game = Game of score:int

module Game =
    let newGame = Game 0

    let score = function Game score' -> score'

    let bowl knockedPins _ = Game knockedPins