module Year2019Day05

open AdventOfCode.Common
open System

let dumpMemory mem =
  mem |> Array.iter(printfn "%d")

let fetch mode value (memory : int array) =
  match mode with
  | 0 -> memory.[value]
  | 1 -> value
  | _ -> invalidArg "mode" "out of range"

let store mode index (memory : int array) accumulator =
  match mode with
  | 0 -> Array.set memory index accumulator
  | _ -> invalidArg "mode" "out of range"

let positionalFlag position flags =
  match position with
  | 1 -> flags % 10
  | 2 -> (flags % 100) / 10
  | 3 -> flags / 100
  | _ -> invalidArg "mode" "out of range"

let fetchParam pc position (memory : int array) = 
  let flags = memory.[pc] / 100
  fetch (positionalFlag position flags) memory.[pc+position] memory

let storeParam pc position (memory : int array) value = 
  let flags = memory.[pc] / 100
  store (positionalFlag position flags) memory.[pc+position] memory value

let executeOperation (data : int array) index =
  let opCode = data.[index]
  match opCode % 100 with
  | 1 -> seq {fetchParam index 1 data; fetchParam index 2 data} |>  Seq.sum |> storeParam index 3 data; (index+4, data)
  | 2 -> seq {fetchParam index 1 data; fetchParam index 2 data} |> Seq.fold(*) 1 |> storeParam index 3 data; (index+4, data)
  | 3 -> printf ">"; Console.ReadLine() |> int |> storeParam index 1 data; (index+2, data)
  | 4 -> fetchParam index 1 data |> printfn "%d"; (index+2, data)
  | 5 -> fetchParam index 1 data |> (fun x -> match x with | 0 -> (index+3, data) | _ -> (fetchParam index 2 data, data))
  | 6 -> fetchParam index 1 data |> (fun x -> match x with | 0 -> (fetchParam index 2 data, data) | _ -> (index+3, data))
  | 7 ->  fetchParam index 1 data - fetchParam index 2 data < 0 |> (fun x -> match x with | true -> 1 | false -> 0) |> storeParam index 3 data; (index+4, data)
  | 8 ->  fetchParam index 1 data - fetchParam index 2 data = 0 |> (fun x -> match x with | true -> 1 | false -> 0) |> storeParam index 3 data; (index+4, data)
  | 99 -> (-1, data)
  | _ -> invalidArg "opCode" "Not a valid opcode"

let rec execute (data : int array) pc =
  match executeOperation data pc with | (-1, _) -> "" | (newPc, data) -> execute data newPc

let solvePart1 (data : array<int>) =
  execute (Array.copy data) 0

let solvePart2 (data : int array) =
  execute (Array.copy data) 0

let solver =
  { parse = parseFirstLine (splitBy "," asIntArray); part1 = solvePart1; part2 = solvePart2}
