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

    // add sand
    // drop to lowest free point -> check point x-1,y-1 -> check point x+1,y-1 -> if point changed repeat
    // check if sand is in first or last column -> done
    // if not repeat
    // map

// make the simulation

// part 1
// getInput 14
getTestInput 14

    // printfn "minx: %d maxx: %d maxy: %d" min
// part 2
getInput 14
// getTestInput 14