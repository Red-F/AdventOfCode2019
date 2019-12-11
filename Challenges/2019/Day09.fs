module Year2019Day09

open AdventOfCode.Common
open System.Collections.Generic

type State = | Halt | IO | Running
type Memory = { Size : int; Real : int64 array; Virtual : Dictionary<int,int64>}
type ExecutionResult = Complete of complete : int64 list | Incomplete of incomplete :  int * int * int64 list

let asInt64Array : string [] -> int64 [] = Array.map int64

let getMemory memory index =
  if index < 0 then failwithf "negative memory can not be accessed (%d)" index
  match index < memory.Size with
  | true -> memory.Real.[index]
  | false -> if memory.Virtual.ContainsKey(index) then memory.Virtual.[index] else 0L

let setMemory memory index  value =
  if index < 0 then failwithf "negative memory can not be accessed (%d)" index
  match index < memory.Size with
  | true -> memory.Real.[index] <- value
  | false -> memory.Virtual.[index] <- value

let fetch mode parameter relBase memory =
  match mode with
  | 0 -> getMemory memory (int parameter)
  | 1 -> parameter
  | 2 -> getMemory memory (relBase + int parameter)
  | _ -> invalidArg "mode" "out of range"

let store mode index relBase memory value =
  match mode with
  | 0 -> setMemory memory (int index) value
  | 2 -> setMemory memory (relBase + int index) value
  | _ -> invalidArg "mode" "out of range"

let positionalFlag position flags =
  match position with
  | 1 -> flags % 10
  | 2 -> (flags % 100) / 10
  | 3 -> flags / 100
  | _ -> invalidArg "position" "out of range"

let executeOperation memory pc relBase inputs outputs =
  let z = getMemory memory (int pc)
  let fetchParam position = fetch (positionalFlag position (int (getMemory memory pc) / 100)) (getMemory memory (pc+position)) relBase memory
  let storeParam position value = store (positionalFlag position (int (getMemory memory pc) / 100)) (getMemory memory (pc+position)) relBase memory value
  let opCode = getMemory memory pc
  match int (opCode % 100L) with
  | 1 -> seq {fetchParam 1; fetchParam 2} |>  Seq.sum |> storeParam 3; (Running, pc+4, relBase, inputs, outputs)
  | 2 -> seq {fetchParam 1; fetchParam 2} |> Seq.fold(*) 1L |> storeParam 3; (Running, pc+4, relBase, inputs, outputs)
  | 3 -> match inputs with | head::tail ->  storeParam 1  head; (Running, pc+2, relBase,  tail, outputs) | [] -> (IO, pc, relBase, inputs, outputs)
  | 4 -> fetchParam 1 |> (fun x -> (Running, pc+2, relBase, inputs, x :: outputs))
  | 5 -> fetchParam 1 |> (fun x -> match x with | 0L -> (Running, pc+3, relBase, inputs, outputs) | _ -> (Running, int (fetchParam 2), relBase, inputs, outputs))
  | 6 -> fetchParam 1 |> (fun x -> match x with | 0L -> (Running, int (fetchParam 2), relBase, inputs, outputs) | _ -> (Running, pc+3, relBase, inputs, outputs))
  | 7 ->  fetchParam 1 - fetchParam 2 < 0L |> (fun x -> match x with | true -> 1L | false -> 0L) |> storeParam 3; (Running, pc+4, relBase, inputs, outputs)
  | 8 ->  fetchParam 1 - fetchParam 2 = 0L |> (fun x -> match x with | true -> 1L | false -> 0L) |> storeParam 3; (Running, pc+4, relBase, inputs, outputs)
  | 9 ->  fetchParam 1 |> (fun x -> (Running, pc+2, int x + relBase, inputs, outputs))
  | 99 -> (Halt, pc, relBase, inputs, outputs)
  | _ -> invalidArg "opCode" "Not a valid opcode"

let rec execute mem pc relBase inputs outputs =
  match executeOperation mem pc relBase inputs outputs with 
  | (Halt, _, _, _, o) -> Complete o
  | (Running, newPc, newRelBase, newInputs, newOutputs) -> execute mem newPc newRelBase newInputs newOutputs
  | (IO, pc, r, _, o) -> Incomplete (pc, r, o)

let solvePart1 data =
  let memory = {Real=data; Size=data.Length; Virtual=new Dictionary<int,int64>()}
  execute memory 0 0 [1L] [] |> (fun x -> match x with | Complete c -> List.head c | _ -> failwith "should have completed")

let solvePart2 data =
  let memory = {Real=data; Size=data.Length; Virtual=new Dictionary<int,int64>()}
  execute memory 0 0 [2L] [] |> (fun x -> match x with | Complete c -> List.head c  | _ -> failwith "should have completed")

let solver =
  { parse = parseFirstLine (splitBy "," asInt64Array); part1 = solvePart1; part2 = solvePart2}