#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System.Text.RegularExpressions
open System
open System.Linq
open System.Collections.Generic
open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent

let getTestInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename)
    // File.ReadAllText(filename)

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename)
    // File.ReadAllText(filename)

type SensorInfo = {SensorX: int; SensorY: int; BeaconX: int; BeaconY: int}
let parseInput (sensorBeaconInput:string) =
    let sensorInput, beaconInput = sensorBeaconInput.Split(": ") |> fun sbi -> sbi[0],sbi[1]
    let sensorX, sensorY = sensorInput.Replace("Sensor at ", "").Split(", ") |> fun si -> int (si[0].Replace("x=", "")), int (si[1].Replace("y=", ""))
    let beaconX, beaconY = beaconInput.Replace("closest beacon is at ", "").Split(", ") |> fun si -> int (si[0].Replace("x=", "")), int (si[1].Replace("y=", ""))

    {SensorX = sensorX;SensorY = sensorY; BeaconX = beaconX; BeaconY = beaconY}
// parse input
// enable range marker for a line

// part 1
// getInput 15
getTestInput 15
|> Array.map parseInput

    // printfn "minx: %d maxx: %d maxy: %d" min
// part 2
// getInput 15
// getTestInput 15