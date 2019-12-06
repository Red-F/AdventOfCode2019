module Year2019Day06

open AdventOfCode.Common

type Planet = { name : string; indirect : int; planets : Planet list }

let asOrbit = splitBy ")" (fun x -> (x.[0], x.[1]))

let rec toConstellation name (depth : int) orbits =
  let inOrbit = orbits |> List.filter(fun (a,_) -> a = name)
  let others = orbits |> List.filter(fun (a,_) -> a <> name)
  match inOrbit with
  | [] -> {name=name; indirect=depth; planets=[]}
  | _ ->  {name=name; indirect=depth; planets= inOrbit |> List.map(fun (_,b) -> toConstellation b (depth+1) others)}

let rec traverseConstellation (p : Planet) =
  match p with
  | {planets=[]}  -> 1 + p.indirect
  | _ ->  p.planets |> List.fold(fun acc elem -> acc + traverseConstellation elem) (p.indirect + 1)

let rec findName s (l: Planet list) (p : Planet) =
  match p with
  | _ when p.name = s -> (true, p :: l |> List.fold(fun acc elem -> elem :: acc ) [])
  | _ when List.isEmpty p.planets -> (false, [])
  | _ -> p.planets |> List.fold(fun (found, planets) elem -> match found with | true -> (found, planets) | false -> findName s (p :: l) elem) (false, l)

let rec orbitalTransfers (l1 : Planet list) (l2 : Planet list) =
  match l1, l2 with
  | h1::t1, h2::t2 when h1 = h2 -> orbitalTransfers t1 t2
  | _::t1, _::t2 -> t1.Length + t2.Length
  | _ -> invalidArg "l1, l2" "do not pas empty list here"

let solvePart1 orbits   =
  toConstellation "COM" -1 (orbits |> Seq.toList) |> traverseConstellation

let solvePart2 orbits =
  let constellation = toConstellation "COM" -1 (orbits |> Seq.toList)
  orbitalTransfers (constellation |> findName "YOU" [] |> snd) (constellation |> findName "SAN" [] |> snd)
  
let solver =
    { parse = parseEachLine asOrbit; part1 = solvePart1; part2 = solvePart2 }
 