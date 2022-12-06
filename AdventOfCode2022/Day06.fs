module Day06

let hasNoDuplicates (w:string[]) = w.Length = (w |> Array.sort |> Array.distinct).Length

let part1 (input:string[]) =
    input 
    |> Seq.map (fun s -> s.ToCharArray() |> Seq.map (fun c -> c.ToString()) |> Seq.windowed 4)
    |> Seq.map (fun row -> (row |> Seq.findIndex hasNoDuplicates) + 4)
    |> Seq.head


let part2 (input:string[]) =
    input 
    |> Seq.map (fun s -> s.ToCharArray() |> Seq.map (fun c -> c.ToString()) |> Seq.windowed 14)
    |> Seq.map (fun row -> (row |> Seq.findIndex hasNoDuplicates) + 14)
    |> Seq.head
    
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

