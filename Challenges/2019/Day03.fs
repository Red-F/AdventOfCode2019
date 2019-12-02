module Year2019Day03

open System
open System.Collections.Generic
open AdventOfCode.Common

let walk (code : string) (thesequence : (int * int * int) seq) =
  let point = if Seq.isEmpty thesequence then (0,0,0) else thesequence |> Seq.last
  let direction = code.[0]
  let count = code.[1..] |> int
  match direction with
  | 'R' -> seq { match point with | (a,b,c) -> for i in 1..count -> (a, b+i, c+i) }
  | 'L' -> seq { match point with | (a,b,c) -> for i in 1..count -> (a, b-i, c+i) }
  | 'U' -> seq { match point with | (a,b,c) -> for i in 1..count -> (a+i, b, c+i) }
  | 'D' -> seq { match point with | (a,b,c) -> for i in 1..count -> (a-i, b, c+i) }
  | _ -> invalidArg "" ""

let wire (codeString : string) =
  printfn "wire"
  let m = new Dictionary<int * int, int>()
  codeString.Split (",", StringSplitOptions.None) |> Seq.fold (fun x y -> walk y x |> Seq.append x ) Seq.empty |> 
  Seq.iter (fun (a,b,c) -> m.TryAdd((a,b),c) |> ignore) |> fun () -> m |> Seq.map (fun (KeyValue((a,b),v)) -> (a,b,v))

let crosses (data : string seq) =
  printfn "crosses"
  data |> Seq.collect(fun x -> wire x |> Seq.map (fun (a,b,_) -> (a,b))) |> Seq.countBy id |> 
  Seq.filter (fun ((a,b), c) -> c > 1) |> Seq.map (fun ((a,b),_) -> (a,b)) |> Seq.cache

let solvePart1 (data : string seq)  =
  printfn "solvePart1"
  crosses data |> Seq.minBy (fun (a,b) -> abs a + abs b) |> (fun (a,b) -> abs a + abs b)

let solvePart2 (data : string seq) =
  printfn "solvePart2"
  let ca = crosses data |> Seq.toArray
  let isCross x = ca |> Array.exists (fun e -> e = x )
  let m = new Dictionary<int * int, int>()
  let addMap (a,b,c) = if m.TryAdd((a,b), c) then () else m.[(a,b)] <- c + m.[(a,b)]
  data |> Seq.collect(wire) |> Seq.filter (fun (a,b,c) -> isCross (a,b)) |> Seq.iter(addMap)
  m |> Seq.minBy (fun (KeyValue(_,v)) -> v) |> (fun (KeyValue (_, v)) -> v)
  
let solver =
    { parse = parseEachLine asString; part1 = solvePart1; part2 = solvePart2 }
 