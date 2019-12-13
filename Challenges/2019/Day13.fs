module Year2019Day13

open AdventOfCode.Common
open System.Collections.Generic

type State = | Halt | IO | Running
type Memory = { Size : int; Real : int64 array; Virtual : Dictionary<int,int64>}

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
  | (Halt, _, _, _, o) -> (0, 0, (o |> List.rev))
  | (Running, newPc, newRelBase, newInputs, newOutputs) -> execute mem newPc newRelBase newInputs newOutputs
  | (IO, pc, r, _, o) -> (pc, r, (o  |> List.rev))

let analyseJoystick (_,b,p) o =
  let triplets = o |> Seq.chunkBySize 3 |> Seq.toList
  let ball =  triplets |> List.filter(fun a -> a.[2] = 4L) |> (fun l -> match l with | [] -> b | _ -> List.head l)
  let paddle =  triplets |> List.filter(fun a -> a.[2] = 3L) |> (fun l -> match l with | [] -> p | _ -> List.head l)
  let move = match ball.[0] = paddle.[0] with | true -> 0L | _ -> match ball.[0] < paddle.[0] with | true -> -1L | false -> 1L
  (move, ball, paddle)

let score o =
  o |> Seq.chunkBySize 3 |> Seq.toList |> List.filter(fun a -> a.[0] = -1L) |> (fun l -> match l with | [] -> 0L | _ -> List.head l |> (fun a -> a.[2]) )  

let playArcade memory =
  let rec play memory pc relBase outputs lastAnalysis =
    let analysis = analyseJoystick lastAnalysis outputs
    let inputs = analysis |> (fun (move, _, _) -> [move])
    match execute memory pc relBase inputs [] with
    | (0, 0, o) -> score o
    | (newPc, newRelBase,o) -> play memory newPc newRelBase o analysis
  execute memory 0 0 [] [] |> (fun (pc, relBase, outputs) -> play memory pc relBase outputs (0L, [|0L|], [|0L|]))

let solvePart1 data  =
  let memory ={Real=data; Size=data.Length; Virtual=new Dictionary<int,int64>()}
  execute memory 0 0 [] [] |> (fun (_,_,o) -> Seq.chunkBySize 3 o |> Seq.countBy(fun x -> x.[2]) |> Seq.filter(fun (a,b) -> a = 2L) |> Seq.head |> snd)

let solvePart2 (data : int64 array) =
  data.[0] <- 2L
  {Real=data; Size=data.Length; Virtual=new Dictionary<int,int64>()} |> playArcade

let asInt64Array : string [] -> int64 [] = Array.map int64

let solver =
    { parse =  parseFirstLine (splitBy "," asInt64Array); part1 = solvePart1; part2 = solvePart2 }