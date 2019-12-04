// Again from https://github.com/CameronAavik/AdventOfCode
//
// I found this a useful way to structure the individual challenges
// into a coherent program

open System
open System.IO
open AdventOfCode.Common
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running


let getSolver year day part printResult =
  let run (solver : Day<'a, 'b, 'c>) =
    let run part solve =
      let fileName = Path.Combine("input-files", year.ToString(), (sprintf "day%02i.txt" day))
      fun _ ->
        let result = fileName |> File.ReadLines |> solver.parse |> solve
        if printResult then
          printfn "Year %i Day %02i-%i %O" year day part result
    match part with
    | 1 -> run 1 solver.part1
    | 2 -> run 2 solver.part2
    | _ -> fun _ -> ()
  match year with
  | 2018 ->
    match day with
    | 1 -> run Year2018Day01.solver | 2 -> run Year2018Day02.solver
    | day -> fun _ -> printfn "Invalid Day: %i (Year %i)" day year
  | 2019 ->
    match day with
    | 1 -> run Year2019Day01.solver | 2 -> run Year2019Day02.solver | 3 -> run Year2019Day03.solver | 4 -> run Year2019Day04.solver | 5 -> run Year2019Day05.solver
    | 6 -> run Year2019Day06.solver | 7 -> run Year2019Day07.solver | 8 -> run Year2019Day08.solver | 9 -> run Year2019Day09.solver | 10 -> run Year2019Day10.solver
    | day -> fun _ -> printf "."
  | year -> fun _ -> printfn "Invalid Year: %i" year

type Bench() =
  let mutable solverFunc : unit -> unit = fun _ -> ()

  [<Params (2019)>]
  member val public Year = 0 with get, set

  // [<Params (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25)>]
  [<Params (4)>]
  member val public Day = 0 with get, set

  [<Params (1, 2)>]
  member val public Part = 0 with get, set

  [<GlobalSetup>]
  member self.GlobalSetupData() =
      solverFunc <- getSolver self.Year self.Day self.Part false

  [<Benchmark>]
  member self.RunPart () = solverFunc ()

  // Usage:  AdventOfCode BENCH     -> benchmark all the solutions for all parts of all days of all years
  //         AdventOfCode ALL       -> run the solutions for all parts of all days of all years
  //         AdventOfCode 2018      -> run the solutions for all parts of all days of 2018
  //         AdventOfCode 2018.12   -> run the solutions for all parts of day 12 of 2018
  //         AdventOfCode 2018.7.2  -> run the solution for part 2 of day 7 of 2018
[<EntryPoint>]
let main argv =
  let runPart year day part = getSolver year day part true ()
  let runDay year day = for part in 1..2 do runPart year day part
  let runYear year = for day in 1..25 do runDay year day
  match argv.[0] with
      | "BENCH" -> BenchmarkRunner.Run<Bench>() |> ignore
      | "ALL" -> for year in 2018..2018 do runYear year
      | x ->
          let parts = x.Split('.') |> Array.map int
          match parts.Length with 
          | 1 -> runYear parts.[0]
          | 2 -> runDay parts.[0] parts.[1]
          | 3 -> runPart parts.[0] parts.[1] parts.[2]
          | _ -> ()
  Console.ReadKey() |> ignore
  0
