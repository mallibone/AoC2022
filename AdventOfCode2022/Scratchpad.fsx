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

let parseInput (input:string[]) =
    input
    |> Array.mapi(fun y row -> 
        row.ToCharArray() 
        // |> Array.map (fun c -> c.ToString())
        |> Array.mapi (fun x c -> ((x,y),(int c, c.ToString())))
    )
    |> Array.collect id
    |> Map.ofArray

let moveToEnd (input:Map<(int*int),(int * string)>) =
    let startPos = input |> Map.findKey (fun pos  h -> snd h = "S")
    let endPos = input |> Map.findKey (fun pos  h -> snd h = "E")

    // findPath
    // gets all possible next moves -> split list and search for new opo
    // checks if move is back on the same path (ignore solution path)
    // checks if move is E -> return successful path and abort others

    (startPos,endPos)
    

// part 1
// getInput 12
getTestInput 12
|> parseInput
|> moveToEnd

// part 2
// getInput 12
// getTestInput 12
