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
    | SubPaket of list<PaketElement>

let rec parsePaket paketInput =
    match paketInput with
    | "[]" -> []
    | _ ->
        // remove braces
        let innerPaketInput = paketInput.Substring(1,paketInput.Length-2)
        let innerPaketElements =
            if innerPaketInput.Contains("[") then
                // this is wrong -> need to take first [ then step into subparser, exit subparser if same level reaches ]
                let innerSubPaketInput = paketInput.Substring(innerPaketInput.IndexOf("["),innerPaketInput.LastIndexOf("]"))
                printfn "%s" innerSubPaketInput
                innerPaketInput.Replace(innerSubPaketInput, "{gnabber}")
            else
                innerPaketInput

        [for paketElement in innerPaketElements.Split(",") do 
            if paketElement = "{gnabber}" then
                yield SubPaket (parsePaket paketElement)
            else
                yield Number (int paketElement)]

let parseInput (packetInput:string) =
    packetInput.Split(System.Environment.NewLine + "" + System.Environment.NewLine)
    |> Array.map (fun pairInput -> pairInput.Split(System.Environment.NewLine) |> Array.map parsePaket)

// part 1
// let gna = 
// getInput 13
getTestInput 13
|> parseInput

// part 2
// getInput 13
// getTestInput 13
