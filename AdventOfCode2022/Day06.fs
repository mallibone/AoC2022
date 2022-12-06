module Day06

open System

let findIndex windowSize (input:string) = 
    input.ToCharArray() 
    |> Seq.windowed windowSize
    |> Seq.findIndex (fun r -> r |> Set.ofArray |> Set.count = windowSize) 
    |> (+) windowSize

let part1 (input:string[]) =
    input 
    |> Seq.map (findIndex 4)
    |> (fun r -> String.Join(",", r))


let part2 (input:string[]) =
    input 
    |> Seq.map (findIndex 14)
    |> (fun r -> String.Join(",", r))
    
let executeDay (testInput:string[]) (input:string[]) =
    // part 1
    testInput
    |> part1
    |> printfn "Part 1 Test: %s"

    input
    |> part1
    |> printfn "Part 1: %s"

    // part 2
    testInput
    |> part2
    |> printfn "Part 2 Test: %s"

    input
    |> part2
    |> printfn "Part 2: %s"

