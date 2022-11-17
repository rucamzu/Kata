module BowlingTests

open Expecto
open FsCheck
open BowlingFs

let private flip f b a = f a b

let private formatBowls = List.map (fun n -> $"{n}") >> String.concat " + "

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
            let expectedScore: int = List.sum bowls
            bowls
            |> List.fold (flip Game.bowl) Game.newGame
            |> Game.score
            |> Expect.equal expectedScore
                $"the score after knocking down {formatBowls bowls} pins on the first frame should be the total amount of {expectedScore} knocked down pins" )

    testPropertyWithConfig config "after consecutive open frames is the total amount of knocked pins"
        (Prop.forAll (Arb.fromGen Gen.openFrames) <| fun bowls ->
            let expectedScore: int = List.sum bowls
            bowls
            |> List.fold (flip Game.bowl) Game.newGame
            |> Game.score
            |> Expect.equal expectedScore
                $"""the score after knocking down {formatBowls bowls} pins on the first frame(s) should be the total amount of {expectedScore} knocked down pins""")

    testPropertyWithConfig config "after a spare bonuses the next bowl"
        (Prop.forAll ([Gen.spare; Gen.firstBowl |> Gen.map List.singleton] |> Gen.collect id |> Gen.map (List.collect id) |> Arb.fromGen) <| fun bowls ->
            let expectedScore: int = (List.sum bowls) + bowls[2]
            bowls
            |> List.fold (flip Game.bowl) Game.newGame
            |> Game.score
            |> Expect.equal expectedScore
                $"""the score after knocking down {formatBowls bowls} pins should bonus the bowl following the spare ({bowls[2]}) for a total of {expectedScore}""")

    testPropertyWithConfig config "after consecutive spares bonuses the bowls following each spare"
        (Prop.forAll ([Gen.spare; Gen.spare; Gen.firstBowl |> Gen.map List.singleton] |> Gen.collect id |> Gen.map (List.collect id) |> Arb.fromGen) <| fun bowls ->
            let expectedScore = (List.sum bowls) + bowls[2] + bowls[4]
            bowls
            |> List.fold (flip Game.bowl) Game.newGame
            |> Game.score
            |> Expect.equal expectedScore
                $"""the score after knocking down {formatBowls bowls} pins should bonus the bowls following each spare ({bowls[2]} + {bowls[4]}) for a total of {expectedScore}""")

    testPropertyWithConfig config "after a strike bonuses the next two bowls"
        (Prop.forAll (Arb.fromGen Gen.openFrame) <| fun bowls ->
            let bowls' = 10 :: bowls
            let expectedScore = 10 + (List.sum bowls) * 2
            bowls'
            |> List.fold (flip Game.bowl) Game.newGame
            |> Game.score
            |> Expect.equal expectedScore
                $"""the score after knocking down {formatBowls bowls'} pins should bonus the two bowls following the strike for a total of {expectedScore}""")

    testPropertyWithConfig config "after consecutive strikes bonuses the next two bowls following each strike"
        (Prop.forAll (Arb.fromGen Gen.openFrame) <| fun bowls ->
            let bowls' = List.append [10; 10; 10] bowls
            let expectedScore = 60 + 3 * bowls[0] + 2 * bowls[1]
            bowls'
            |> List.fold (flip Game.bowl) Game.newGame
            |> Game.score
            |> Expect.equal expectedScore
                $"""the score after knocking down {formatBowls bowls'} pins should bonus the two bowls following each strike for a total of {expectedScore}""")

]