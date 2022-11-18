open Expecto
open Hopac
open Logary
open Logary.Configuration
open Logary.Adapters.Facade
open Logary.Targets

[<EntryPoint>]
let main argv =
    Config.create "BowlingTests" "localhost"
    |> Config.targets [ LiterateConsole.create LiterateConsole.empty "console" ]
    |> Config.processing (Events.events |> Events.sink ["console";])
    |> Config.build
    |> run
    |> LogaryFacadeAdapter.initialise<Expecto.Logging.Logger>

    runTestsInAssemblyWithCLIArgs [] argv
