module Year2019Day12

open AdventOfCode.Common
open System.Text.RegularExpressions

let delta a b = if a = b then 0 else if a < b then 1 else -1

let pull3D (x,y,z) (a,b,c) = (delta x a, delta y b, delta z c)

let others p positions = seq { for x in positions do if p <> x then yield x }

let deltaV3D p positions = 
  others p positions |> Seq.map(pull3D p) |> Seq.fold(fun (a1,a2,a3) (e1,e2,e3) -> (a1+e1,a2+e2,a3+e3)) (0,0,0)

let deltaV p (positions : int array) = 
  others p positions |> Seq.sumBy(delta p)

let gravity3D positions (velocities : (int*int*int) array) =
  let deltas = seq { for p in positions do yield p} |> Seq.map(fun x -> deltaV3D x positions) |> Seq.toArray
  Array.map2(fun (v1,w1,u1) (v2,w2,u2) -> (v1+v2,w1+w2,u1+u2)) velocities deltas

let gravity (positions : int array) (velocities : int array) =
  let deltas = seq { for p in positions do yield p} |> Seq.map(fun x -> deltaV x positions) |> Seq.toArray
  Array.map2(+) velocities deltas

let move3D positions velocities =
  Array.map2(fun (x,y,z) (v,w,u) -> (x+v,y+w,z+u)) positions velocities

let move (positions : int array) (velocities : int array) =
  Array.map2(+) positions velocities

let rec simulate3D count positions velocities =
  match count with
  | 1000 -> (positions, velocities)
  | _ -> gravity3D positions velocities |> (fun newVelocities -> simulate3D (count+1 ) (move3D positions newVelocities) newVelocities)

let rec simulate count refPositions refVelocities (positions : int array) (velocities : int array) =
  match positions = refPositions && velocities = refVelocities && count > 0 with
  | true -> (count, positions, velocities)
  | false -> gravity positions velocities |> (fun newVelocities -> simulate (count+1 ) refPositions refVelocities (move positions newVelocities) newVelocities)

let planeCycle (positions : int array) =
  let velocities = Array.create positions.Length 0
  simulate 0 (Array.copy positions) (Array.copy velocities) positions velocities |> (fun (x,_,_) -> x)

let rec ggdEuclides (a : int64) (b : int64) =
  match a % b with | 0L -> b | r -> match ( r > b) with | true -> ggdEuclides r b | false -> ggdEuclides b r

let kleinstGemeneVeelvoud (a : int64) (b : int64) =
  let ggd = match a > b with | true -> ggdEuclides a b | false -> ggdEuclides b a
  (a * b) / ggd

let kinetic3D (positions, velocities) =
  Array.map2(fun (x,y,z)(v,w,u) -> (abs x + abs y + abs z) * (abs v + abs w + abs u)) positions velocities |> Array.sum

let solvePart1 data  =
  let positions = data |> Seq.toArray
  let velocities = Array.create positions.Length (0,0,0)
  simulate3D 0 positions velocities |> kinetic3D

let solvePart2 data =
  let cycleX = planeCycle (data |> Seq.toArray |> Array.map(fun (x,_,_) -> x)) |> int64
  let cycleY = planeCycle (data |> Seq.toArray |> Array.map(fun (_,y,_) -> y)) |> int64
  let cycleZ = planeCycle (data |> Seq.toArray |> Array.map(fun (_,_,z) -> z)) |> int64
  kleinstGemeneVeelvoud (kleinstGemeneVeelvoud cycleX cycleY) cycleZ

let asPosition (line : string) =
    let regexPattern = @"<x=(-?\d+),\s*y=(-?\d+),\s*z=(-?\d+)>"
    let m = Regex.Match(line, regexPattern)
    let gs = [ for g in m.Groups -> g.Value ] |> List.tail
    (int gs.[0], int gs.[1], int gs.[2])

let solver =
    { parse =  parseEachLine asPosition; part1 = solvePart1; part2 = solvePart2 }