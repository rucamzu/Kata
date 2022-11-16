module BowlingTests

open Expecto
open Expecto.Flip
open FsCheck
open BowlingFs

module Gen =
    let firstBowl = Gen.choose(0, 10)

[<Tests>]
let ScoreTests = testList "score" [

    test "of a new game is zero" {
        Game.newGame
        |> Game.score
        |> Expect.equal "the score of a new game should be zero" 0
    }

    testProperty "after bowling once is the amomunt of knocked pins"
        (Prop.forAll (Arb.fromGen Gen.firstBowl) <| fun knockedPins ->
            Game.newGame
            |> Game.bowl knockedPins
            |> Game.score
            |> Expect.equal $"the score after knocking down {knockedPins} pin(s) on the first bowl should be the number of knocked down pins: {knockedPins}" knockedPins)

    test "after an open frame is the total amount of knocked pins" {
        Game.newGame
        |> Game.bowl 3
        |> Game.bowl 5
        |> Game.score
        |> Expect.equal $"the score after an open frame should be the total amount number of knocked down pins:" 8
    }

]