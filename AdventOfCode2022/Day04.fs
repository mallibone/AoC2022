module Day04

let parseRangeInput (rangeInput:string) =
    rangeInput.Split("-") |> (fun i -> [(int i[0]) .. (int i[1])])

let parseInput (assignmentPair:string) =
    assignmentPair.Split(",") |> Seq.map parseRangeInput |> Seq.toList

let hasTotalOverlap (pair:int list list) =
    let firstPair = pair[0]
    let secondPair = pair[1]
    let containsAll fp sp = fp |> Seq.forall (fun f -> sp |> Seq.contains f)

    containsAll firstPair secondPair || containsAll secondPair firstPair

let hasPartialOverlap (pair:int list list) =
    let firstPair = pair[0]
    let secondPair = pair[1]
    firstPair |> Seq.exists (fun f -> secondPair |> Seq.contains f)


let part1 input =
    input
    |> Seq.map parseInput
    |> Seq.filter hasTotalOverlap
    |> Seq.length


let part2 input =
    input
    |> Seq.map parseInput
    |> Seq.filter hasPartialOverlap
    |> Seq.length
    
let executeDay (testInput:string[]) (input:string[]) =
    // part 1
    testInput
    |> part1
    |> printfn "Part 1 Test: %d"

    input
    |> part1
    |> printfn "Part 1: %d"

    // part 2
    testInput
    |> part2
    |> printfn "Part 2 Test: %d"

    input
    |> part2
    |> printfn "Part 2: %d"

