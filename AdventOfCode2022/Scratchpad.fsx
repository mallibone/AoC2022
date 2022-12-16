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

// Parse all the movements
let parseCoords (input:string) =
    input.Split(" -> ") 
    |> Seq.map (fun s -> s.Split(",") |> (fun [|x;y|] -> int x, int y))

let drawShapesOnMap (map:string[,]) (shapeLineCoords:(int*int)[]) =
    let x1,y1 = fst shapeLineCoords[0], snd shapeLineCoords[0]
    let x2,y2 = fst shapeLineCoords[1], snd shapeLineCoords[1]
    let shapeCoords =
        // vertical line
        if x1 = x2 then [for i in y1 .. y2 do yield (x1,i)]
        // draw horizontal line
        else [for i in x2 .. x1 do yield (i,y1)]
    for (x,y) in shapeCoords do map[y,x] <- "#"
    map

let getMap shapeCoords =
    let minXCoord = (shapeCoords |> Seq.collect id |> Seq.minBy fst |> fst)
    let maxXCoord = (shapeCoords |> Seq.collect id |> Seq.maxBy fst |> fst)
    let maxYCoord = (shapeCoords |> Seq.collect id |> Seq.maxBy snd |> snd)

    let map = Array2D.create ((maxYCoord) + 1) (maxXCoord-minXCoord + 1) "."

    let mapWithShapes =
        shapeCoords 
        |> Seq.map (fun c -> c |> Seq.windowed 2)
        |> Seq.collect id
        |> Seq.map (fun w -> w |> Array.map (fun (x,y) -> x-minXCoord,y))
        |> Seq.fold drawShapesOnMap map
    mapWithShapes

let rec simulateSand sandSourceCoord map =
    let xSource = fst sandSourceCoord
    // add sand
    // drop to lowest free point -> check point x-1,y-1 -> check point x+1,y-1 -> if point changed repeat
    // check if sand is in first or last column -> done
    // if not repeat
    map

// make the simulation

// part 1
let sandSourceCoord = (500,0)
// getInput 14
getTestInput 14
|> Seq.map parseCoords
|> getMap


    // printfn "minx: %d maxx: %d maxy: %d" min
// part 2
// getTestInput 14
// getInput 14
