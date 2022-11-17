module BowlingTests

open Expecto
open FsCheck
open BowlingFs

let private flip f b a = f a b

let private config = { FsCheckConfig.defaultConfig with startSize = 1; endSize = 10; maxTest = 100 }

module private Expect =
    let equal expected message actual = Expecto.Expect.equal actual expected message

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
    let spare =
        Gen.choose(0, 10)
        |> Gen.listOfLength 2
        |> Gen.filter (List.head >> ((>) 10))
        |> Gen.filter (List.sum >> ((=) 10))
        

[<Tests>]
let ScoreTests = testList "score" [

    test "of a new game is zero" {
        Game.newGame
        |> Game.score
        |> Expect.equal 0
            "the score of a new game should be zero"
    }

    testPropertyWithConfig config "after bowling once is the amount of knocked pins"
        (Prop.forAll (Arb.fromGen Gen.firstBowl) <| fun knockedPins ->
            Game.newGame
            |> Game.bowl knockedPins
            |> Game.score
            |> Expect.equal knockedPins
                $"the score after knocking down {knockedPins} pin(s) on the first bowl should be the amount of knocked down pins: {knockedPins}")

    testPropertyWithConfig config "after an open frame is the total amount of knocked pins"
        (Prop.forAll (Arb.fromGen Gen.openFrame) <| fun bowls ->
            bowls
            |> List.fold (flip Game.bowl) Game.newGame
            |> Game.score
            |> Expect.equal (List.sum bowls)
                $"the score after knocking down {bowls[0]} and {bowls[1]} pins on the first frame should be the total amount of knocked down pins: {List.sum bowls}" )

    testPropertyWithConfig config "after consecutive open frames is the total amount of knocked pins"
        (Prop.forAll (Arb.fromGen Gen.openFrames) <| fun bowls ->
            bowls
            |> List.fold (flip Game.bowl) Game.newGame
            |> Game.score
            |> Expect.equal (List.sum bowls)
                $"""the score after knocking down {bowls |> List.map (fun n -> $"{n}") |> String.concat " + "} pins on the first frame(s) should be the total amount of knocked down pins: {List.sum bowls}""")

    testPropertyWithConfig config "after a spare bonuses the next bowl"
        (Prop.forAll ([Gen.spare; Gen.firstBowl |> Gen.map List.singleton] |> Gen.collect id |> Gen.map (List.collect id) |> Arb.fromGen) <| fun bowls ->
            bowls
            |> List.fold (flip Game.bowl) Game.newGame
            |> Game.score
            |> Expect.equal (bowls |> List.sum |> (+) bowls[2])
                $"""the score after a {bowls |> List.take 2 |> List.map (fun n -> $"{n}") |> String.concat " + "} spare should bonus the score of the following bowl: {bowls |> List.sum |> (+) bowls[2]}""")

]