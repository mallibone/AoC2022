#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System
open System.Collections.Generic
open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

let parseInput textInput =
    let rec parser result input =
        match input with
        | [||] -> result |> Seq.rev
        | _ when input[0] = "" -> parser result (input |> Array.skip 1)
        | _ ->
            let calories = input |> Seq.takeWhile (fun line -> line <> "") |> Seq.map int |> Seq.sum
            parser (calories::result) (input |> Seq.skipWhile(fun line -> line <> "") |> Seq.toArray)
    parser [] textInput

// part 1
// getTestInput 1
getInput 1
|> parseInput
|> Seq.sortByDescending id
|> Seq.head 

// part 2
getInput 1
|> parseInput
|> Seq.sortByDescending id
|> Seq.take 3 
|> Seq.sum