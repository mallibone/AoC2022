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
    // File.ReadAllLines(filename)
    File.ReadAllText(filename)

let getInput (day:int) =
    let filename = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    // File.ReadAllLines(filename)
    File.ReadAllText(filename)

type PaketElement =
    | Number of int
    | Paket of list<PaketElement>

let parsePaket (paketInput:string) =
    let rec parsePaketElements paketInput (parsedElements:list<PaketElement>) =
        printfn "PI %A PE %A" paketInput parsedElements
        match paketInput with
        | [] -> [], parsedElements
        | headElement::tailElements ->
            printfn "HE %s" headElement
            if headElement.Contains("[") then
                printfn "["
                let newTail, subPaket = (parsePaketElements (headElement.Substring(1,headElement.Length-1)::tailElements) [])
                let newParsedElements = parsedElements@subPaket

                parsePaketElements newTail newParsedElements
            elif headElement.Contains("]") then
                // Wrap up Sub-Paket
                printfn "]h %s" (headElement.Substring(0,headElement.Length - 1))
                let indx = headElement.IndexOf("]")
                let newParsedElements = 
                    if indx <> 0 then 
                        let newElement = (headElement.Substring(0, indx)) |> int |> Number
                        parsedElements@[newElement] 
                    else
                        parsedElements

                printfn "t %A" tailElements
                let newTail = if indx < (headElement.Length - 1)  then headElement.Substring(indx+1)::tailElements else tailElements
                printfn "nt %A" newTail
                newTail, [Paket newParsedElements]
            else
                printfn "#"
                let newParsedElements = if not (String.IsNullOrEmpty(headElement)) then parsedElements@[Number (int headElement)] else parsedElements@[Paket []]
                parsePaketElements tailElements newParsedElements

    parsePaketElements (paketInput.Substring(1,paketInput.Length-2).Split(",") |> Array.toList) []

let parseInput (packetInput:string) =
    packetInput.Split(System.Environment.NewLine + "" + System.Environment.NewLine)
    |> Array.map (fun pairInput -> pairInput.Split(System.Environment.NewLine)  |> Array.map parsePaket)

// part 1
// let gna = 
// getInput 13
getTestInput 13
|> parseInput
// let foo = gna[0][0]

// part 2
// getInput 13
// getTestInput 13
