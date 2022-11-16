module BowlingTests

open Expecto
open Expecto.Flip
open FsCheck
open BowlingFs

module Gen =
    let firstBowl = Gen.choose(0, 10)

[<Tests>]
let BowlingTests = testList "bowling" [

    test "new game starts with zero score" {
        Game.newGame
        |> Game.score
        |> Expect.equal "the score of a new game should be zero" 0
    }

    testProperty "score after one bowling is the number of knocked pins"
        (Prop.forAll (Arb.fromGen Gen.firstBowl) <| fun knockedPins ->
            Game.newGame
            |> Game.bowl knockedPins
            |> Game.score
            |> Expect.equal $"the score after knocking down {knockedPins} pin(s) on the first bowl should be the number of knocked down pins: {knockedPins}" knockedPins)

]