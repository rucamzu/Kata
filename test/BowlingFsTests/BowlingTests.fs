module BowlingTests


open Expecto
open Expecto.Flip
open BowlingFs

[<Tests>]
let BowlingTests = testList "bowling" [

    test "new game starts with zero score" {
        Game.newGame
        |> Game.score
        |> Expect.equal "the score of a new game should be zero" 0
    }

]