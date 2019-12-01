module Year2019Day01

open AdventOfCode.Common

let fuelRequiredForModule mass =
  (mass / 3) - 2

let rec fuelRequiredForModuleAndFuel mass =
  if fuelRequiredForModule mass <= 0 then 0
  else fuelRequiredForModule mass + fuelRequiredForModuleAndFuel (fuelRequiredForModule mass)

let solvePart1 modules  =
  modules |> Seq.sumBy fuelRequiredForModule

let solvePart2 modules =
  modules |> Seq.sumBy fuelRequiredForModuleAndFuel
  
let solver =
    { parse = parseEachLine asInt; part1 = solvePart1; part2 = solvePart2 }
