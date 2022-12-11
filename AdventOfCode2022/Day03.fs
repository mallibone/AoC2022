module Day03

// Shared
let scoreCard = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".ToCharArray() |> Array.mapi (fun i v -> v, i+1)

let toPriority item =
    scoreCard |> Seq.find (fun (code, value) -> code = item) |> snd

// Part 1
let parsePart1 (rucksackContents:string) =
    let middle = rucksackContents.Length / 2
    rucksackContents.Substring(0,middle), rucksackContents.Substring(middle, middle)

let findDuplicates (firstCompartment:string, secondCompartment:string) =
    let itemsOfFirstCompartment = firstCompartment.ToCharArray()
    let itemsOfSecondCompartment = secondCompartment.ToCharArray()
    itemsOfFirstCompartment |> Seq.find (fun i -> itemsOfSecondCompartment |> Seq.contains i)

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

let part1 input =
    input
    |> Seq.map parsePart1
    |> Seq.map findDuplicates
    |> Seq.map toPriority
    |> Seq.sum

let part2 input =
    input
    |> makeGroups
    |> List.map getGroupBadge
    |> Seq.map toPriority
    |> Seq.sum
    
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
