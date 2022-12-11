module Day1X

open Common

let part1 input =
    input 
    |> Array.length

let part2 input =
    input 
    |> Array.length

let executeDay day =
    // part 1
    getTestInputAllText day
    |> part1
    |> printfn "Part 1 Test: %d"

    getInputAllText day
    |> part1
    |> printfn "Part 1: %d"

    // part 2
    getTestInputAllText day
    |> part2
    |> printfn "Part 2 Test: %d"

    getInputAllText day
    |> part2
    |> printfn "Part 2: %d"
