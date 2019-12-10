module Year2019Day08

open AdventOfCode.Common
open System.Text.RegularExpressions

let width = 25
let tall = 6
let layerSize = (width * tall)

let toGroups l (s : string) = 
  Seq.init (s.Length / l) (fun n -> s.[n*l..(n*l)+(l-1)])

let countMatches m s = Regex.Matches(s, Regex.Escape m).Count

let mergeLayer (acc : string) (elem : string) =
  String.mapi(fun i c -> match elem.[i] with | '2' -> c | _ -> elem.[i]) acc

let printImage rows =
  rows |> Seq.iter(fun s -> s |> String.iter(fun c -> match c with | '0' -> printf " " | '1' -> printf "@" | _ -> failwithf "invalid character %c in map" c); printfn "")

let solvePart1 digits  =
  toGroups layerSize digits |> Seq.sortBy(fun s -> countMatches "0"  s) |> Seq.head |> (fun s -> (countMatches "1" s) * (countMatches "2" s))

let solvePart2 digits =
  printfn ""
  toGroups layerSize digits |> Seq.toList |> List.rev |> List.fold( mergeLayer) (String.init layerSize (fun x -> "2")) |> toGroups width |> printImage
  printfn ""
  ""
  
let solver =
    { parse = parseFirstLine asString; part1 = solvePart1; part2 = solvePart2 }
 