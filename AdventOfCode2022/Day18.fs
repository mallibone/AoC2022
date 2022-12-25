module Day18

open Common
open System.Linq
open FSharp.Collections.ParallelSeq
open System.Collections.Generic

let parseInput (inputLine:string) =
    inputLine.Split(",") |> Array.map int |> fun c -> (c[0],c[1],c[2]),0

let parseInputPartII (inputLine:string) =
    inputLine.Split(",") |> Array.map int |> fun c -> c[0],c[1],c[2]

let getNeighbours (x,y,z) =
    (x,y,z), [x-1,y,z;x+1,y,z;x,y-1,z;x,y+1,z;x,y,z-1;x,y,z+1]

let sumFreeSides (map:Dictionary<int*int*int,int>) =
    map.Keys 
    |> PSeq.map getNeighbours
    |> PSeq.iter (fun (key, neighbours) -> 
        let freeSides = neighbours |> Seq.map (fun n -> not (map.Keys.Contains(n))) |> Seq.filter id |> Seq.length
        map[key] <- freeSides)
    map.Values |> Seq.sum

let draw3dMap coords =
    let maxX = coords |> Seq.maxBy (fun (x,_,_) -> x) |> fun (x,_,_) -> x
    let maxY = coords |> Seq.maxBy (fun (_,y,_) -> y) |> fun (_,y,_) -> y 
    let maxZ = coords |> Seq.maxBy (fun (_,_,z) -> z) |> fun (_,_,z) -> z

    Array3D.init (maxX + 1) (maxY + 1) (maxZ + 1) (fun x y z -> coords |> Seq.contains (x,y,z))

let exists droplet (x, y, z) =
    if x < 0 || y < 0 || z < 0 then
        false
    elif x > ((droplet |> Array3D.length1) - 1)
         || y > ((droplet |> Array3D.length2) - 1)
         || z > ((droplet |> Array3D.length3) - 1) then
        false
    else
        droplet[x, y, z]

// kudos: https://github.com/jovaneyck/advent-of-code-2022/blob/main/day%2018/part2.fsx#L78
let rec floodfill dropletCoords queue visited =
    match queue with
    | [] -> visited
    | c :: cs ->
        if visited |> Set.contains c then
            floodfill dropletCoords cs visited
        else
            let reachableNeighbours =
                getNeighbours c |> snd
                //Bounding box
                |> List.filter (fun (x, y, z) -> x >= -1 && y >= -1 && z >= -1)
                |> List.filter (fun (x, y, z) ->
                    x <= (dropletCoords |> Array3D.length1) + 1
                    && y <= (dropletCoords |> Array3D.length2) + 1
                    && z <= (dropletCoords |> Array3D.length3) + 1)
                |> List.filter (fun n -> not (exists dropletCoords n))

            floodfill dropletCoords (List.append cs reachableNeighbours) (visited |> Set.add c)


let sumFreeSidesPartII (coords:list<int*int*int>) =
    let dropCoords = draw3dMap coords
    let fillCoords =
        floodfill dropCoords [0,0,0] Set.empty
        |> Set
    
    coords 
    |> PSeq.map (fun c -> (c |> (getNeighbours >> snd)) |> Set |> Set.intersect fillCoords |> Set.count)
    |> Seq.sum


let part1 input =
    input 
    |> Array.map parseInput
    |> dict |> (fun d -> Dictionary(d) )
    |> sumFreeSides

let part2 input =
    input 
    |> Seq.map parseInputPartII
    |> Seq.toList
    |> sumFreeSidesPartII

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
