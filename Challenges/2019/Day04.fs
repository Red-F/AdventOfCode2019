module Year2019Day04

open System
open AdventOfCode.Common

let doubled (c : char) =
  String.Concat(c, c)

let hasAdjacentDigits (s : string) =
  s |> Seq.toList |> Seq.fold (fun acc elem ->  acc || s.IndexOf(doubled elem) >= 0) false

let hasExactlyDoubleDigits (s : string) =
  let exactlyDoubleChar (str : string) c = str.IndexOf(doubled c) >= 0 && str.IndexOf(doubled c) = str.LastIndexOf(doubled c)
  s |> Seq.toList |> Seq.fold (fun acc elem -> acc || exactlyDoubleChar s elem) false

let generateWithIncreasingDigitsOnly (range : int array) =
  let mutable index = range.[0]
  let foldIt (s : string, f) elem =
    match (s,f) with
    | ("", _) -> (string elem, false)
    | (_, true) -> ((s + string s.[s.Length-1], true))
    | _ -> if elem >= s.[s.Length-1] then (s + string elem, false) else (s + string s.[s.Length-1], true)
  seq {
  while index < range.[1] do
    index <- index |> string |> Seq.fold (foldIt)("", false) |> fst |> int
    if index < range.[1] then yield index
    index <- index + 1 }

let solvePart1 (range : int array)  =
  generateWithIncreasingDigitsOnly range |> Seq.map(string) |> Seq.filter(hasAdjacentDigits) |> Seq.length

let solvePart2 (range : int array) =
  generateWithIncreasingDigitsOnly range |> Seq.map(string) |> Seq.filter(hasExactlyDoubleDigits) |> Seq.length

let solver =
    { parse = parseFirstLine (splitBy "-" asIntArray); part1 = solvePart1; part2 = solvePart2 }
