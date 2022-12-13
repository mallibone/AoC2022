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

type MapField = {Value : int; StringValue : string; Visited : bool}
let parseInput (input:string[]) =
    input
    |> Array.mapi(fun y row -> 
        row.ToCharArray() 
        // |> Array.map (fun c -> c.ToString())
        |> Array.mapi (fun x c -> ((x,y),{Value = (if c = 'E' then 123 else int c); StringValue = c.ToString(); Visited = false}))
    )
    |> Array.collect id
    |> Map.ofArray

let findNeighbours maxX maxY pos =
    let x = fst pos
    let y = snd pos
    let neighbourCandidates =
        // [(x-1,y+1);(x,y+1);(x+1,y+1);
        // (x-1,y);           (x+1,y);
        // (x-1,y-1);(x,y-1);(x+1,y-1)]
        [          (x,y+1);          
        (x-1,y);           (x+1,y);
                  (x,y-1)          ]
        |> List.filter(fun (cx,cy) -> cx >= 0 && cy >= 0 && cx <= maxX && cy <= maxY)
    // printfn "%A" neighbourCandidates
    neighbourCandidates

let canMove startPos (map:Map<(int*int),MapField>) currentPos nextPos =
    // printfn "canMove: %A %A %A" (map |> Map.find currentPos).Value (map |> Map.find nextPos).Value (not (map |> Map.find nextPos).Visited)
    currentPos = startPos 
    || (Math.Abs((map |> Map.find currentPos).Value - (map |> Map.find nextPos).Value) <= 1
        && (not (map |> Map.find nextPos).Visited))

let moveToEnd (map:Map<(int*int),MapField>) =
    let maxX = map |> Map.keys |> Seq.maxBy fst |> fst
    let maxY = map |> Map.keys |> Seq.maxBy snd |> snd
    let nextNeighbour = (findNeighbours maxX maxY)
    let startPos = map |> Map.findKey (fun _  h -> h.StringValue = "S")
    let endPos = map |> Map.findKey (fun _  h -> h.StringValue = "E")
    let cCanMove = canMove startPos


    let rec makeMove map (paths:list<list<int*int>>) oldPaths =
        let newPaths =
            paths
            |> List.collect (
                fun p -> 
                    // printfn "%A" p
                    (nextNeighbour (p |> List.head))
                    |> List.filter (fun n -> 
                        // printfn "IsCandidate: %A && %A" (not (p |> List.contains n)) (canMove map (p |> Seq.head) n)
                        // (not (paths |> List.collect id |> List.contains n)) && (cCanMove map (p |> Seq.head) n))
                        // (not (paths |> List.exists (fun pp -> pp |> List.contains n))) && (cCanMove map (p |> Seq.head) n))
                        (not (p |> List.contains n)) && (cCanMove map (p |> Seq.head) n))
                    |> List.map(fun n -> n::p))
            // |> PSeq.toList

        let hasFoundTheEnd =
            newPaths |> List.tryFind(fun (lastMove::_) -> lastMove = endPos)

        if hasFoundTheEnd |> Option.isSome then
            printfn "%A" paths
            hasFoundTheEnd.Value
        else
            let highestPaths, newOldPaths= 
                if newPaths |> List.length <> 0 then
                        let hPaths =
                            newPaths 
                            |> List.map (fun p -> p.Head)
                            |> List.maxBy (fun p -> (map |> Map.find p).Value)
                            |> (fun p -> (map |> Map.find p).Value)
                            |> (fun hv -> newPaths |> List.filter (fun np -> (map |> Map.find np.Head).Value = hv))
                        let oPaths = oldPaths
                        hPaths, oPaths
                else
                    oldPaths |> List.head,oldPaths |> List.tail

            // printfn "hp: %A" highestPos
            // let distinctNewPaths = 
            //     newPaths 
                // |> List.distinctBy (fun np -> np |> Seq.head)
            // let distinctNewPaths =
            //     if distinctNewPaths |> List.exists (fun dnp -> dnp |> List.length >= 3) then
            //         distinctNewPaths
            //         |> List.filter(fun dnp -> (dnp |> List.take 3) |> List.forall(fun npp -> paths |> List.forall (fun pp -> pp |> List.contains npp)))
            //     else
            //         distinctNewPaths

            // printfn "%d %A" (newPaths |> List.length) newPaths
            // printfn "%d/%d" (distinctNewPaths |> List.length) (newPaths |> List.length)
            printfn "%d/%d" (newPaths |> List.length) (highestPaths |> List.length)
            if highestPaths.Length = 0 then printfn "%A" newPaths


            // let setValue (v:MapField Option) = 
            let setValue v = 
                match v with
                | Some s -> Some {s with Visited = true}
                | None -> None
            // System.Threading.Thread.Sleep(1000)
            let newMap =
                map
                // newPaths
                // |> Seq.map (fun np -> np |> Seq.head)
                // |> Seq.fold (fun m nph -> (m |> Map.change nph setValue)) map
            
            // if (newPaths |> Seq.length) <> 0 then
            if (highestPaths |> Seq.length) <> 0 then
                makeMove newMap highestPaths newOldPaths
            else
                printfn "path %A" paths
                printfn "newpath %A" highestPaths
                []
    
    makeMove map [[startPos]] [[[]]]

    // findPath
    // gets all possible next moves -> split list and search for new opo
    // checks if move is back on the same path (ignore solution path)
    // checks if move is E -> return successful path and abort others

    // (startPos,endPos)
    

// part 1
// let gna = 
getInput 12
// getTestInput 12
|> parseInput
|> moveToEnd
|> List.length |> fun i -> i - 1

// part 2
// getInput 12
// getTestInput 12
