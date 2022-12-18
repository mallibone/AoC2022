module Day14

open Common
open System

// Parse all the movements
let parseCoords (input:string) =
    input.Split(" -> ") 
    |> Seq.map (fun s -> s.Split(",") |> (fun c -> int c[0], int c[1]))

let drawShapesOnMap (map:string[,]) (shapeLineCoords:(int*int)[]) =
    let x1,y1 = fst shapeLineCoords[0], snd shapeLineCoords[0]
    let x2,y2 = fst shapeLineCoords[1], snd shapeLineCoords[1]
    let shapeCoords =
        // vertical line
        if x1 = x2 then 
            if y1 < y2 then [for i in y1 .. y2 do yield (x1,i)]
            else [for i in y2 .. y1 do yield (x1,i)]
        // draw horizontal line
        else 
            if x2 < x1 then [for i in x2 .. x1 do yield (i,y1)]
            else [for i in x1 .. x2 do yield (i,y1)]
    for (x,y) in shapeCoords do map[y,x] <- "#"
    map

let getMap (sourceX,sourceY) shapeCoords =
    let minXCoord = (shapeCoords |> Seq.collect id |> Seq.minBy fst |> fst) - 1
    let maxXCoord = (shapeCoords |> Seq.collect id |> Seq.maxBy fst |> fst)
    let maxYCoord = (shapeCoords |> Seq.collect id |> Seq.maxBy snd |> snd) + 1

    let map = Array2D.create ((maxYCoord) + 1) (maxXCoord-minXCoord + 2) "."

    let mapWithShapes =
        shapeCoords 
        |> Seq.map (fun c -> c |> Seq.windowed 2)
        |> Seq.collect id
        |> Seq.map (fun w -> w |> Array.map (fun (x,y) -> x-minXCoord,y))
        |> Seq.fold drawShapesOnMap map

    let shiftedSourceCoords = (sourceX-minXCoord,sourceY)
    mapWithShapes, shiftedSourceCoords

let drawMapFloor map =
    let maxYCoord = (map |> Array2D.length1) - 1
    let newMap = map |> Array2D.copy
    for x in [0 .. (map |> Array2D.length2) - 1] do newMap[maxYCoord,x] <- "#"
    newMap

let getMapPart2 (sourceX,sourceY) shapeCoords =
    let minXCoord = (shapeCoords |> Seq.collect id |> Seq.minBy fst |> fst) - 1
    let maxXCoord = (shapeCoords |> Seq.collect id |> Seq.maxBy fst |> fst)
    let maxYCoord = (shapeCoords |> Seq.collect id |> Seq.maxBy snd |> snd) + 2

    let map = Array2D.create ((maxYCoord) + 1) (maxXCoord-minXCoord + 2) "."

    let mapWithShapes =
        shapeCoords 
        |> Seq.map (fun c -> c |> Seq.windowed 2)
        |> Seq.collect id
        |> Seq.map (fun w -> w |> Array.map (fun (x,y) -> x-minXCoord,y))
        |> Seq.fold drawShapesOnMap map
        |> drawMapFloor

    let shiftedSourceCoords = (sourceX-minXCoord,sourceY)
    mapWithShapes, shiftedSourceCoords

let mapIsFree (map:string[,]) x y = 
    map[y,x] = "."

let printMap (map:string[,]) =
    printfn "***"
    for i in [0 .. (map |> Array2D.length1) - 1] do
        printfn "%s" (String.Join(" ", map[i,*]))
    printfn "***"

let rec simulateSand round (map,sandSourceCoord ) =
    let xSource = (fst sandSourceCoord)
    let ySource = snd sandSourceCoord
    let maxY = (map |> Array2D.length1) - 1
    let maxX = (map |> Array2D.length2) - 1
    let mapIsFree = mapIsFree map    

    let rec recFindSandPos x y =
        let sx, sy = x, ([y .. maxY] |> List.takeWhile (fun i -> mapIsFree x i) |> List.max)
        if sx > 0 && sy < maxY && mapIsFree (sx-1) (sy+1) then
            recFindSandPos (sx-1) (sy+1)
        elif sx < maxX && sy < maxY && mapIsFree (sx+1) (sy+1) then
            recFindSandPos (sx+1) (sy+1)
        else sx,sy
    
    let (sx,sy) = recFindSandPos xSource ySource
    map[sy,sx] <- "o"

    if sx = 0 || sx = maxX || sy = maxY then
        // printMap map
        round
    else
        simulateSand (round + 1) (map,sandSourceCoord)

let rec simulateSandPart2 round (map,sandSourceCoord ) =
    let xSource = (fst sandSourceCoord)
    let ySource = snd sandSourceCoord
    let maxY = (map |> Array2D.length1) - 1
    let maxX = (map |> Array2D.length2) - 1
    let mapIsFree = mapIsFree map    

    let rec recFindSandPos x y =
        let sx, sy = x, ([y .. maxY] |> List.takeWhile (fun i -> mapIsFree x i) |> List.max)
        if sx > 0 && sy < maxY && mapIsFree (sx-1) (sy+1) then
            recFindSandPos (sx-1) (sy+1)
        elif sx < maxX && sy < maxY && mapIsFree (sx+1) (sy+1) then
            recFindSandPos (sx+1) (sy+1)
        else sx,sy
    
    let (sx,sy) = recFindSandPos xSource ySource
    map[sy,sx] <- "o"

    // printfn "%d %d" sx sy

    if (sx,sy) = sandSourceCoord then
        // printMap map
        round + 1
    elif sx = 0 || sx = maxX then
        let mapIncrease = 20
        let offsetShift = (mapIncrease-2)/2
        let newMap = 
            Array2D.create (maxY + 1) (maxX + mapIncrease) "."
            |> drawMapFloor
        newMap[0 .. maxY, offsetShift .. maxX+offsetShift] <- map
        simulateSandPart2 (round + 1) (newMap,(xSource + offsetShift, ySource))
    else
        simulateSandPart2 (round + 1) (map,sandSourceCoord)


let sandSourceCoord = (500,0)

let part1 input =
    input 
    |> Seq.map parseCoords 
    |> getMap sandSourceCoord
    |> simulateSand 0

let part2 input =
    input 
    |> Seq.map parseCoords
    |> getMapPart2 sandSourceCoord
    |> simulateSandPart2 0

let executeDay day =
    // part 1
    getTestInputLines day
    |> part1
    |> printfn "Part 1 Test: %d"

    getInputLines day
    |> part1
    |> printfn "Part 1: %d"

    // part 2
    getTestInputLines day
    |> part2
    |> printfn "Part 2 Test: %d"

    getInputLines day
    |> part2
    |> printfn "Part 2: %d"
