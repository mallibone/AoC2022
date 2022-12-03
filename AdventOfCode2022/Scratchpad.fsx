#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
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

// Part 1
let parsePart1 (rucksackContents:string) =
    let middle = rucksackContents.Length / 2
    rucksackContents.Substring(0,middle), rucksackContents.Substring(middle, middle)

let findDuplicates (firstCompartment:string, secondCompartment:string) =
    let itemsOfFirstCompartment = firstCompartment.ToCharArray()
    let itemsOfSecondCompartment = secondCompartment.ToCharArray()
    itemsOfFirstCompartment |> Seq.find (fun i -> itemsOfSecondCompartment |> Seq.contains i)

let scoreCard = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".ToCharArray() |> Array.mapi (fun i v -> v, i+1)

let toPriority item =
    scoreCard |> Seq.find (fun (code, value) -> code = item) |> snd

// Part 2
let makeGroups backpacks =
    let rec groupMaker result remainingBackpacks =
        match remainingBackpacks with
        | [] -> result
        | _ -> groupMaker (result@[(remainingBackpacks |> List.take 3)]) (remainingBackpacks |> List.skip 3)
    groupMaker [] (backpacks |> Array.toList)

let getGroupBadge (groupPacks:list<string>) =
    let itemsFirstPack = groupPacks[0].ToCharArray()
    let itemsSecondPack = groupPacks[1].ToCharArray()
    let itemsThirdPack = groupPacks[2].ToCharArray()

    let overlapingItems = itemsFirstPack |> Seq.filter (fun i -> itemsSecondPack |> Seq.contains i)
    itemsThirdPack |> Seq.find (fun i -> overlapingItems |> Seq.contains i)

// part 1
getInput 3
// getTestInput 3
|> Seq.map parsePart1
|> Seq.map findDuplicates
|> Seq.map toPriority
|> Seq.sum

// part 2
getInput 3
// getTestInput 3
|> makeGroups
|> List.map getGroupBadge
|> Seq.map toPriority
|> Seq.sum