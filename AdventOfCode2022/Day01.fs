module Day01
open Common

let parseInput textInput =
    let rec parser result input =
        match input with
        | [||] -> result |> Seq.rev
        | _ when input[0] = "" -> parser result (input |> Array.skip 1)
        | _ ->
            let calories = input |> Seq.takeWhile (fun line -> line <> "") |> Seq.map int |> Seq.sum
            parser (calories::result) (input |> Seq.skipWhile(fun line -> line <> "") |> Seq.toArray)
    parser [] textInput

let part1 (input:string[]) =
    input
    |> parseInput
    |> Seq.sortByDescending id
    |> Seq.head 

let part2 (input:string[]) =
    input
    |> parseInput
    |> Seq.sortByDescending id
    |> Seq.take 3 
    |> Seq.sum
    
let executeDay (day:int) =
    // part 1
    getTestInputLines day
    |> part1
    |> printfn "Part 1 Test: %d"

    getInputLines day
    |> part1
    |> printfn "Part 1: %d"

    // part 2
    Common.getTestInputLines day
    |> part2
    |> printfn "Part 2 Test: %d"

    getInputLines day
    |> part2
    |> printfn "Part 2: %d"
    
