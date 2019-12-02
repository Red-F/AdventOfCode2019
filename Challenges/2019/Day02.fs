module Year2019Day02

open AdventOfCode.Common

let assemble (data : int array) index =
  match data.[index] with
  | 1 -> Array.set data data.[index+3] (data.[data.[index+1]] + data.[data.[index+2]]); (4, data)
  | 2 -> Array.set data data.[index+3] (data.[data.[index+1]] * data.[data.[index+2]]); (4, data)
  | 99 -> (0, data)
  | _ -> invalidArg "opCode" "Not a valid opcode"

let rec execute data pc =
  match assemble data pc with | (0, data) -> data.[0] | (size, data) -> execute data (pc + size)

let solve noun verb data =
  Array.set data 1 noun
  Array.set data 2 verb
  execute data 0

let solvePart1 (data : array<int>) =
  solve 12 2 data

let solvePart2 output (data : int array) =
  seq { for i in 0 .. data.Length-1 do for j in 0 .. data.Length-1 -> (i, j) } |>
  Seq.filter (fun (verb, noun) -> solve noun verb (Array.copy data) = output) |>
  Seq.map (fun (verb, noun ) -> noun*100 + verb) |> Seq.head

let solver =
  { parse = parseFirstLine (splitBy "," asIntArray); part1 = solvePart1; part2 = solvePart2 19690720}
