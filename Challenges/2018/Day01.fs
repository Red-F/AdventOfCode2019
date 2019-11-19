module Year2018Day01

open AdventOfCode.Common

let repeatSequence data = seq { while true do yield! data }

let runningTotal data = Seq.scan (+) 0 data |> Seq.skip 2

let potentialDuplicates pos value period data =
  data |> Array.choose (fun x -> if x - value <= 0 then None else match ((x - value) % period) with | 0 -> Some (pos, x, (x - value) / period) | _ -> None)

let solvePart2 data =
    let deltas = data |> Seq.toArray
    let sum = deltas |> Array.sum
    let relevantTotals = repeatSequence data |> runningTotal |> Seq.take deltas.Length |> Seq.toArray
    relevantTotals |> Array.mapi (fun i x -> potentialDuplicates (i+1) x sum relevantTotals) |> Array.concat |> 
      Array.sortBy (fun (position, _, repeatCount) -> (repeatCount * sum) + position) |>
      Array.map (fun (_, value, _) -> value) |> Array.head

let solver =
    { parse = parseEachLine asInt; part1 = Seq.sum; part2 = solvePart2 }

(*  -- this is the non-optimised initial version
    -- easier to read, but ten times slower

module Year2018Day01

open AdventOfCode.Common

let repeatSequence deltas =
    seq { while true do yield! deltas }

let runningTotal data = 
  Seq.scan (+) 0 data |> Seq.skip 1

let filterDuplicates (data : seq<int>) =
    let h = System.Collections.Generic.HashSet()
    seq { for i in data do match h.Add i with | true -> () | false -> yield i }

let solvePart2 deltas =
    repeatSequence deltas |> runningTotal |> filterDuplicates |> Seq.head

let solver =
    { parse = parseEachLine asInt; part1 = Seq.sum; part2 = solvePart2 }

*)