module BowlingTests

open Expecto
open Expecto.Flip
open FsCheck
open BowlingFs

let private flip f b a = f a b

let private config = { FsCheckConfig.defaultConfig with startSize = 1; endSize = 10; maxTest = 100 }

module Gen =
    let firstBowl = Gen.choose(0, 10)
    let openFrame =
        Gen.choose(0, 10)
        |> Gen.listOfLength 2
        |> Gen.filter (List.sum >> ((>) 10))
    let openFrames =
        openFrame
        |> Gen.listOf
        |> Gen.map (List.collect id)
        

[<Tests>]
let ScoreTests = testList "score" [

    test "of a new game is zero" {
        Game.newGame
        |> Game.score
        |> Expect.equal "the score of a new game should be zero" 0
    }

    testProperty "after bowling once is the amount of knocked pins"
        (Prop.forAll (Arb.fromGen Gen.firstBowl) <| fun knockedPins ->
            Game.newGame
            |> Game.bowl knockedPins
            |> Game.score
            |> Expect.equal $"the score after knocking down {knockedPins} pin(s) on the first bowl should be the amount of knocked down pins: {knockedPins}" knockedPins)

    testProperty "after an open frame is the total amount of knocked pins"
        (Prop.forAll (Arb.fromGen Gen.openFrame) <| fun bowls ->
            bowls
            |> List.fold (flip Game.bowl) Game.newGame
            |> Game.score
            |> Expect.equal $"the score after knocking down {bowls[0]} and {bowls[1]} pins on the first frame should be the total amount of knocked down pins: {List.sum bowls}" (List.sum bowls))

    testProperty "after consecutive open frames is the total amount of knocked pins"
        (Prop.forAll (Arb.fromGen Gen.openFrames) <| fun bowls ->
            bowls
            |> List.fold (flip Game.bowl) Game.newGame
            |> Game.score
            |> Expect.equal $"""the score after knocking down {bowls |> List.map (fun n -> $"{n}") |> String.concat " + "} pins on the first frame(s) should be the total amount of knocked down pins: {List.sum bowls}""" (List.sum bowls))

]