module Day15

open Common
open System

type SensorInfo = {SensorX: int; SensorY: int; BeaconX: int; BeaconY: int; DistanceToBeacon: int}

let distance (p1x:int,p1y:int) (p2x,p2y) = Math.Abs(p1x - p2x) + Math.Abs(p1y - p2y)

let parseInput (sensorBeaconInput:string) =
    let sensorInput, beaconInput = sensorBeaconInput.Split(": ") |> fun sbi -> sbi[0],sbi[1]
    let sensorX, sensorY = sensorInput.Replace("Sensor at ", "").Split(", ") |> fun si -> int (si[0].Replace("x=", "")), int (si[1].Replace("y=", ""))
    let beaconX, beaconY = beaconInput.Replace("closest beacon is at ", "").Split(", ") |> fun si -> int (si[0].Replace("x=", "")), int (si[1].Replace("y=", ""))

    let distanceToBeacon = distance (sensorX,sensorY) (beaconX,beaconY)
    {SensorX = sensorX;SensorY = sensorY; BeaconX = beaconX; BeaconY = beaconY; DistanceToBeacon = distanceToBeacon}

type Map = {MinX: int; MinY: int; MaxX: int; MaxY: int}
let getMapPositions sensorInfos =
    let xMin = sensorInfos |> Seq.map (fun s -> s.SensorX - s.DistanceToBeacon) |> Seq.min
    let yMin = sensorInfos |> Seq.map (fun s -> s.SensorY - s.DistanceToBeacon) |> Seq.min
    let xMax = sensorInfos |> Seq.map (fun s -> s.SensorX + s.DistanceToBeacon) |> Seq.max
    let yMax = sensorInfos |> Seq.map (fun s -> s.SensorY + s.DistanceToBeacon) |> Seq.max
    {MinX = xMin; MinY=yMin;MaxX=xMax;MaxY=yMax}, sensorInfos

let coverCoords map row (mapRow:string[]) sensorInfo =
    if sensorInfo.SensorY = row then mapRow[sensorInfo.SensorX - map.MinX] <- "S"
    if sensorInfo.BeaconY = row then mapRow[sensorInfo.BeaconX - map.MinX] <- "B"
    if Math.Abs(row - sensorInfo.SensorY) <= sensorInfo.DistanceToBeacon then 
        let xFieldsToMark = sensorInfo.DistanceToBeacon - Math.Abs(row - sensorInfo.SensorY)
        for x in [0 .. xFieldsToMark] do 
            if mapRow[sensorInfo.SensorX - map.MinX + x] = "." then mapRow[sensorInfo.SensorX - map.MinX + x] <- "#"
            if mapRow[sensorInfo.SensorX - map.MinX - x] = "." then mapRow[sensorInfo.SensorX - map.MinX - x] <- "#"
    mapRow

let nonBeaconFields row (map,sensorInfos) =
    let mapRow = Array.create (map.MaxX - map.MinX) "."

    sensorInfos |> Seq.fold (coverCoords map row) mapRow

let lineFromTo from dest =
    if from = dest then
        Seq.singleton from
    else
        let xf,yf= from
        let xd,yd = dest
        let xPath = seq {xf .. sign (xd - xf) .. xd}
        let yPath = seq {yf .. sign (yd - yf) .. yd}
        Seq.zip xPath yPath

let getBorderCoords maxCoord sensorInfo =
    let rightX = sensorInfo.SensorX + sensorInfo.DistanceToBeacon + 1
    let topY = sensorInfo.SensorY + sensorInfo.DistanceToBeacon + 1
    let leftX = sensorInfo.BeaconX - sensorInfo.DistanceToBeacon - 1
    let bottomY = sensorInfo.BeaconY - sensorInfo.DistanceToBeacon - 1

    [leftX,sensorInfo.SensorY; sensorInfo.SensorX,topY; rightX,sensorInfo.SensorY; sensorInfo.SensorX,bottomY;leftX,sensorInfo.SensorY]
    |> Seq.pairwise
    |> Seq.collect (fun (from,dest) -> lineFromTo from dest)
    |> Seq.filter (fun (xc,yc) -> xc >= 0 && xc <= maxCoord && yc >= 0 && yc <= maxCoord)

let findDistressBeacon maxCoord sensorInfos =
    let beaconCoords = sensorInfos |> Seq.map(fun si -> si.BeaconX, si.BeaconY) |> Set
    let borderCoords = sensorInfos |> Seq.collect (getBorderCoords maxCoord)
    let distressBeaconCoord = 
        borderCoords 
        |> Seq.filter (fun pos -> 
                        not (beaconCoords |> Seq.contains pos) 
                        && sensorInfos |> Seq.forall (fun si -> (distance (si.SensorX,si.SensorY) pos) > si.DistanceToBeacon))
    distressBeaconCoord


let part1 maxCoord input =
    input 
    |> Array.map parseInput
    |> getMapPositions
    |> nonBeaconFields maxCoord
    |> Seq.filter (fun s -> s = "#")
    |> Seq.length

let part2 maxCoord input =
    input 
    |> Array.map parseInput
    |> findDistressBeacon maxCoord
    |> Seq.head
    |> fun (x,y) -> int64 x * 4_000_000L + int64 y

let executeDay day =
    // part 1
    getTestInputLines day
    |> part1 10
    |> printfn "Part 1 Test: %d"

    getInputLines day
    |> part1 2000000
    |> printfn "Part 1: %d"

    // part 2
    getTestInputLines day
    |> part2 20
    |> printfn "Part 2 Test: %d"

    getInputLines day
    |> part2 4_000_000
    |> printfn "Part 2: %d"
