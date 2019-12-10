module Year2019Day10

open AdventOfCode.Common
open System

let dumpSights (x : (int * int) seq) =
  x |> Seq.iter(fun (a,b) -> printfn "(%d,%d) " a b)

type Space = Empty | Asteroid
let charToSpace =
  function
  | '.' -> Empty
  | '#' -> Asteroid
  | c -> failwithf "invalid character %c in map" c

let parseAsteroids x = x |> Array2D.map charToSpace

let unblockedSights (a : Space [,]) (x,y) =
  seq { for i in 1..a |> Array2D.length1 do for j in 1..a |> Array2D.length2 do if (j-1,i-1) <> (x,y) then yield (j-1-x,i-1-y)}
  |> Seq.sortBy(fun (a,b) -> abs a + abs b) |> Seq.toList

let isAsteroid (s : Space [,]) (x,y) (a,b) = s.[y+b, x+a] = Asteroid

let isBlockedBy (x,y) (a,b) =
  match (x,y),(a,b) with
  | (0,_),(0,_) -> y * b > 0
  | (_,0),(_,0) -> x * a > 0
  | (0,_),_ | _,(0,_) | (_,0),_ | _,(_,0) -> false
  | _ when x * a < 0 || y * b < 0 -> false
  | _ -> b * x = a * y

let isBlockedByList (acc : (int * int) list) (a,b) =
  acc |> List.exists(fun (x,y) -> isBlockedBy (x,y) (a,b))

let scan acc elem = match isBlockedByList acc elem with | true -> acc | false -> elem :: acc

let scanAll (arr : Space[,]) =
    let map j i v =
      match v with
      | Empty -> (0, (i,j))
      | Asteroid -> unblockedSights arr (i,j) |> List.filter(isAsteroid arr (i,j)) |> List.fold(scan)[] |> List.length |> (fun x -> x,(i,j))
    arr |> Array2D.mapi map

let optimalAsteroid asteroids =
  asteroids |> scanAll |> (fun a -> seq { for i in 0..a.GetLength(0)-1 do for j in 0..a.GetLength(1)-1 do yield (a.[i,j])})
  |> Seq.sortByDescending(fun (v,_) -> v) |> Seq.head

let angle (a,b) =
  ((((Math.Atan2(float(b),float(a)) *180.0) / Math.PI) + 450.0) % 360.0)

let solvePart1 asteroids  =
  asteroids |> optimalAsteroid |> (fun (v,_) -> v)

let solvePart2 asteroids =
  let (_,(i,j)) = asteroids |> optimalAsteroid
  unblockedSights asteroids (i,j) |> List.filter(isAsteroid asteroids (i,j)) |> List.fold(scan)[] |> List.rev |> List.map(fun (a,b) -> (angle(a,b),(a+i,b+j)))
  |> List.sortBy(fun (v,_) -> v) |> List.item(199) |> (fun (v,(a,b)) -> (a * 100) + b)

let solver =
    { parse =  parseAsteroids << array2D << (parseEachLine asString); part1 = solvePart1; part2 = solvePart2 }
