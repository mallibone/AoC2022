module Day08

let parseInput (line:string) =
    line.ToCharArray() |> Array.map (fun c -> int (c.ToString()))


let isVisibleTree (grid:int[][]) visibleTrees (hi, wi) =
    let colCoordsUp = [for i in [0 .. hi - 1] do yield grid[i][wi]]
    let colCoordsDown = [for i in [hi+1 .. (grid |> Array.length) - 1] do yield  grid[i][wi]]
    let rowCoordsLeft = [for i in [0 .. wi - 1] do yield grid[hi][i]]
    let rowCoordsRight = [for i in [wi + 1 .. (grid[0] |> Array.length) - 1] do yield grid[hi][i]]
    let sightLines = [colCoordsUp;colCoordsDown;rowCoordsLeft;rowCoordsRight]

    let shorterNeighbours =
        sightLines 
        |> Seq.filter (fun l -> l |> Seq.forall (fun t -> t < grid[hi][wi]))
        |> Seq.length
    if shorterNeighbours > 0 then grid[hi][wi]::visibleTrees else visibleTrees

let findVisibleTrees grid =
    let height = grid |> Array.length
    let width = grid[0] |> Array.length

    let coords =
        [for hi in [1 .. height - 2 ] do
            for wi in [1 .. width - 2 ] do
                yield hi, wi]
    let treesOnEdge = height * 2 + (width - 2) * 2
    coords 
    |> List.fold (isVisibleTree grid) []
    |> List.length
    |> (+) treesOnEdge

let scenicScore (grid:int[][]) treeScores (hi, wi) =
    let colCoordsUp = ([(hi - 1) .. -1 .. 1] |> Seq.takeWhile (fun i -> grid[i][wi] < grid[hi][wi]) |> Seq.length)
    let colCoordsDown = ([hi+1 .. ((grid |> Array.length) - 2)] |> Seq.takeWhile (fun i -> grid[i][wi] < grid[hi][wi]) |> Seq.length)
    let rowCoordsLeft = ([(wi - 1) .. -1 .. 1] |> Seq.takeWhile (fun i -> grid[hi][i] < grid[hi][wi]) |> Seq.length)
    let rowCoordsRight = ([wi + 1 .. (grid[0] |> Array.length) - 2] |> Seq.takeWhile (fun i -> grid[hi][i] < grid[hi][wi]) |> Seq.length)

    let scenicScore = [colCoordsUp;colCoordsDown;rowCoordsLeft;rowCoordsRight] |> Seq.map (fun d -> d+1) |> Seq.reduce (*)

    scenicScore::treeScores

let highestScenicTree grid =
    let height = grid |> Array.length
    let width = grid[0] |> Array.length

    let coords =
        [for hi in [1 .. height - 2 ] do
            for wi in [1 .. width - 2 ] do
                yield hi, wi]

    coords 
    |> List.fold (scenicScore grid) []
    |> List.max

let part1 input =
    input 
    |> Array.map parseInput
    |> findVisibleTrees

let part2 input =
    input 
    |> Array.map parseInput
    |> highestScenicTree

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

