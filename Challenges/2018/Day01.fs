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
