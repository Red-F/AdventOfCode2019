module Year2018Day02

open AdventOfCode.Common

let exactlyNIdentical n data =
  data |> 
  Seq.fold(fun acc s ->  s |> Seq.countBy id |> Seq.filter (fun (_,y) -> y = n) |>  Seq.length |> function 0 -> acc | _ -> acc + 1 ) 0

let solvePart1 data =
  (exactlyNIdentical 2 data) * (exactlyNIdentical 3 data)

let solvePart2 data =
  data |> Seq.collect (fun s -> Seq.init (String.length s) (fun n -> s.[0..n-1] + "." + s.[n+1..])) |> 
  Seq.countBy id |> Seq.find (fun (_, count) -> count = 2 ) |> fst |> (fun s -> s.Replace (".",""))

let solver =
    { parse = parseEachLine asString; part1 = solvePart1; part2 = solvePart2 }
