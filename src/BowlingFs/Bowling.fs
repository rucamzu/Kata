namespace BowlingFs

type Game = Game of unit

module Game =
    let newGame = Game ()

    let score = function Game _ -> 0