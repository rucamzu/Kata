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

    test "score after one bowling is the number of knocked pins" {
        Game.newGame
        |> Game.bowl 7
        |> Game.score
        |> Expect.equal "the score after one single bowl should be the number of knocked down pins" 7
    }

]