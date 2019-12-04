module Year2019Day04

open System
open AdventOfCode.Common

let isIncreasing (s : string) =
  let ctoi c = int c - int '0'
  s |> Seq.toList |> Seq.fold (fun (d, f) elem -> (ctoi elem, f && ctoi elem >= d)) (-1, true) |> snd

let doubled (c : char) =
  String.Concat(c, c)

let hasAdjacentDigits (s : string) =
  s |> Seq.toList |> Seq.fold (fun acc elem ->  acc || s.IndexOf(doubled elem) >= 0) false

let hasExactlyDoubleDigits (s : string) =
  let exactlyDoubleChar (str : string) c = str.IndexOf(doubled c) >= 0 && str.IndexOf(doubled c) = str.LastIndexOf(doubled c)
  s |> Seq.toList |> Seq.fold (fun acc elem -> acc || exactlyDoubleChar s elem) false

let solvePart1 (range : int array)  =
  seq {for i in range.[0]..range.[1] -> string i} |> Seq.filter(fun s -> isIncreasing s && hasAdjacentDigits s) |> Seq.length

let solvePart2 (range : int array) =
  seq {for i in range.[0]..range.[1] -> string i} |> Seq.filter(fun s -> isIncreasing s && hasExactlyDoubleDigits s) |> Seq.length

let solver =
    { parse = parseFirstLine (splitBy "-" asIntArray); part1 = solvePart1; part2 = solvePart2 }
