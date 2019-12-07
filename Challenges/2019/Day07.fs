module Year2019Day07

open AdventOfCode.Common

type State = | Halt | IO | Running
type ExecutionResult = Complete of complete : int list | Incomplete of incomplete :  int * int list

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

let executeOperation (memory : int array) pc inputs (outputs : int list) =
  let fetchParam position = fetch (positionalFlag position (memory.[pc] / 100)) memory.[pc+position] memory
  let storeParam position value = store (positionalFlag position (memory.[pc] / 100)) memory.[pc+position] memory value
  let opCode = memory.[pc]
  match opCode % 100 with
  | 1 -> seq {fetchParam 1; fetchParam 2} |>  Seq.sum |> storeParam 3; (Running, pc+4, inputs, outputs)
  | 2 -> seq {fetchParam 1; fetchParam 2 } |> Seq.fold(*) 1 |> storeParam 3; (Running, pc+4, inputs, outputs)
  | 3 -> match inputs with | head::tail ->  storeParam 1  head; (Running, pc+2,  tail, outputs) | [] -> (IO, pc, inputs, outputs)
  | 4 -> fetchParam 1 |> (fun x -> (Running, pc+2, inputs, x :: outputs))
  | 5 -> fetchParam 1 |> (fun x -> match x with | 0 -> (Running, pc+3, inputs, outputs) | _ -> (Running, fetchParam 2, inputs, outputs))
  | 6 -> fetchParam 1 |> (fun x -> match x with | 0 -> (Running, fetchParam 2, inputs, outputs) | _ -> (Running, pc+3, inputs, outputs))
  | 7 ->  fetchParam 1 - fetchParam 2 < 0 |> (fun x -> match x with | true -> 1 | false -> 0) |> storeParam 3; (Running, pc+4, inputs, outputs)
  | 8 ->  fetchParam 1 - fetchParam 2 = 0 |> (fun x -> match x with | true -> 1 | false -> 0) |> storeParam 3; (Running, pc+4, inputs, outputs)
  | 99 -> (Halt, pc, inputs, outputs)
  | _ -> invalidArg "opCode" "Not a valid opcode"

let rec execute (data : int array) pc (inputs : int list) outputs =
  match executeOperation data pc inputs outputs with 
  | (Halt, _, _, o) -> Complete o
  | (Running, newPc, newInputs, newOutputs) -> execute data newPc newInputs newOutputs
  | (IO, pc, _, o) -> Incomplete (pc, o)

let distrib e l =
    let rec aux pre post = seq {match post with | [] -> yield (l @ [e]) | h::t -> yield (List.rev pre @ [e] @ post); yield! aux (h::pre) t}
    aux [] l

let rec perms = function 
    | [] -> Seq.singleton []
    | h::t -> Seq.collect (distrib h) (perms t)

let initialiseAmp memory phase =
  match execute memory 0 [phase] [] with
  | Incomplete(pc,outputs) -> (memory, pc, outputs)
  | _ -> invalidArg "initialiseAmp" "amplifier should ask input after initialision"

let executeAmp (mem : int array, pc : int, inputs : int list) =
  match execute mem pc inputs [] with Incomplete(newpc,outputs) -> (mem, newpc, outputs) | Complete outputs -> (mem, 0, outputs)

let executeAmps (signal : int ) (amps : (int array * int * int list) list) =
  List.scan(fun (_,_,o) (mem, pc, _) -> executeAmp (mem, pc, o)) ([||], 0, [signal]) amps |> List.tail

let ampsOutput (amps : (int array * int * int list) list) =
  amps |> List.rev |> List.head  |> function | (_,_,head::_) -> head | _ -> invalidArg  "ampInSeries" "unexpected empty list"

let ampInSeries (memory : int array) (phases : int list) =
  phases |> List.map(fun x -> initialiseAmp (Array.copy memory) x) |> executeAmps 0 |> ampsOutput

let ampInFeedbackLoop (memory : int array) (phases : int list) =
  let rec loopback (signal : int ) (amps : (int array * int * int list) list) =
    match executeAmps signal amps with
    | (amps : (int array * int * int list) list) when amps |> List.forall(fun(_,pc,_) -> pc = 0) -> amps
    | amps -> loopback (ampsOutput amps) amps
  phases |> List.map(fun x -> initialiseAmp (Array.copy memory) x) |> loopback 0 |> ampsOutput

let solvePart1 (data : array<int>) =
  perms [0;1;2;3;4] |> Seq.map(ampInSeries data) |> Seq.sortDescending |> Seq.head

let solvePart2 (data : int array) =
  perms [5;6;7;8;9] |> Seq.map(ampInFeedbackLoop data) |> Seq.sortDescending |> Seq.head

let solver =
  { parse = parseFirstLine (splitBy "," asIntArray); part1 = solvePart1; part2 = solvePart2}