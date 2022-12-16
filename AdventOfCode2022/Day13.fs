module Day13

open Common
open System

type PacketElement =
    | Number of int
    | Packet of list<PacketElement>

let parsePacket (packetInput:string) =
    let rec parsePacketElements packetInput (parsedElements:list<PacketElement>) =
        // printfn "PI %A PE %A" packetInput parsedElements
        match packetInput with
        | [] -> [], parsedElements
        | (headElement:string)::tailElements ->
            if headElement.Contains("[") then
                // printfn "["
                let newTail, subPacket = (parsePacketElements (headElement.Substring(1,headElement.Length-1)::tailElements) [])
                let newParsedElements = parsedElements@subPacket

                parsePacketElements newTail newParsedElements
            elif headElement.Contains("]") then
                // Wrap up Sub-Packet
                // printfn "]h %s" (headElement.Substring(0,headElement.Length - 1))
                let indx = headElement.IndexOf("]")
                let newParsedElements = 
                    if indx <> 0 then 
                        let newElement = (headElement.Substring(0, indx)) |> int |> Number
                        parsedElements@[newElement] 
                    else
                        parsedElements

                let newTail = if indx < (headElement.Length - 1)  then headElement.Substring(indx+1)::tailElements else tailElements
                newTail, [Packet newParsedElements]
            else
                let newParsedElements = if not (String.IsNullOrEmpty(headElement)) then parsedElements@[Number (int headElement)] else parsedElements@[Packet []]
                parsePacketElements tailElements newParsedElements

    let inputString, packets = parsePacketElements (packetInput.Substring(1,packetInput.Length-2).Split(",") |> Array.toList) []
    inputString, packets//, packetInput

let parseInput (packetInput:string) =
    packetInput.Split(System.Environment.NewLine + "" + System.Environment.NewLine)
    |> Seq.map (fun pairInput -> pairInput.Split(System.Environment.NewLine)  |> Seq.map (parsePacket >> fun(_,p) -> p) |> Seq.toList)
    |> Seq.toList

let rec findFirstNumber packet =
    match packet with
    | Packet p -> 
        if p |> List.isEmpty then
            -1
        else
            findFirstNumber (p |> List.head)
    | Number n -> n

// let packetFilter (packets:list<list<PacketElement>*string>) =
let packetFilter firstPacket secondPacket =
    let rec recPacketFilter (firstPacket:list<PacketElement>) (secondPacket:list<PacketElement>) =
        // printfn "Filtering %A %A" firstPacket secondPacket
        match firstPacket, secondPacket with
        | Number n1::_, Number n2::_ when n1 < n2 -> Some true
        | Number n1::_, Number n2::_ when n1 > n2 -> Some false
        | Number n1::firstTail, Number n2::secondTail when n1 = n2 -> 
            recPacketFilter firstTail secondTail
        | [],[] -> None//Some true
        | [],_ -> Some true
        | _,[] -> Some false
        | Packet p1::firstTail, Packet p2::secondTail ->
            let subPacketResult = recPacketFilter p1 p2 
            if subPacketResult |> Option.isSome then subPacketResult
            else recPacketFilter firstTail secondTail
        | Packet p1::firstTail, Number n2::secondTail ->
            let subPacketResult = recPacketFilter p1 [Number n2]
            if subPacketResult |> Option.isSome then subPacketResult
            else recPacketFilter firstTail secondTail
        | Number n1::firstTail, Packet p2::secondTail ->
            let subPacketResult = recPacketFilter [Number n1] p2
            if subPacketResult |> Option.isSome then subPacketResult
            else recPacketFilter firstTail secondTail
        | _ -> None

    (recPacketFilter firstPacket secondPacket) //|> Option.defaultWith (fun _ -> false)// |> Option.get

let sortPackets firstPacket secondPacket =
    match packetFilter firstPacket secondPacket with
    | Some true -> -1
    | Some false -> 1
    | None -> 0

let part1 input =
    input 
    |> parseInput
    |> List.indexed
    |> List.filter (fun (_,(p:list<list<PacketElement>>)) -> (packetFilter p[0] p[1]) |> Option.get)
    |> List.map (fst >> ((+) 1))
    |> List.sum 

let part2 input =
    let seperator2 = [Packet [Number 2]]
    let seperator6 = [Packet [Number 6]]
    let seperators = [seperator2;seperator6]

    input 
    |> parseInput
    |> List.append [seperators]
    |> List.collect id
    |> List.sortWith sortPackets
    // |> List.rev
    |> List.indexed
    // |> List.map (fun (x,(_,z)) -> x,z)
    |> List.filter(fun l -> seperators |> Seq.contains (snd l))
    |> List.map (fst >> ((+) 1))
    |> List.reduce (*) 

let executeDay day =
    // part 1
    getTestInputAllText day
    |> part1
    |> printfn "Part 1 Test: %d"

    getInputAllText day
    |> part1
    |> printfn "Part 1: %d"

    // part 2
    getTestInputAllText day
    |> part2
    |> printfn "Part 2 Test: %d"

    getInputAllText day
    |> part2
    |> printfn "Part 2: %d"
