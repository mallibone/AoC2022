#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System.Text.RegularExpressions;
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
    let colCoordsUp = ([0 .. hi - 1] |> Seq.takeWhile (fun i -> grid[i][wi] > i) |> Seq.length)
    let colCoordsDown = ([hi+1 .. (grid |> Array.length) - 1] |> Seq.takeWhile (fun i -> grid[i][wi] > i) |> Seq.length)
    let rowCoordsLeft = ([0 .. wi - 1] |> Seq.takeWhile (fun i -> grid[hi][i] > i) |> Seq.length)
    let rowCoordsRight = ([wi + 1 .. (grid[0] |> Array.length) - 1] |> Seq.takeWhile (fun i -> grid[hi][i] > i) |> Seq.length)

    let scenicScore = colCoordsUp * colCoordsDown * rowCoordsLeft * rowCoordsRight
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


// part 1
getInput 8
// getTestInput 8
|> Array.map parseInput
|> findVisibleTrees


// part 2
// getInput 8
getTestInput 8
|> Array.map parseInput
|> highestScenicTree
