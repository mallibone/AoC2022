module Day10

open System
open System.Linq

let parseInput (input:string) =
    let commandArgs = input.Split(" ")
    match commandArgs[0] with
    | "noop" -> [0]
    | "addx" ->
        let value = int commandArgs[1]
        [0;value]
    | _ -> failwith "Illegal argument"

let boosterCycles = [20; 60; 100; 140; 180; 220]
let changeSignalStrength (signalStrength, index, cycleCheckpoints) (currentSignalChange:int) =
    let newSignalStrength = signalStrength+currentSignalChange
    
    let cycleCheckpoints = 
        if boosterCycles.Contains(index) then 
            (signalStrength*index)::cycleCheckpoints 
        else cycleCheckpoints

    (newSignalStrength,(index+1),cycleCheckpoints)

let emptyScreen() =
    [for i in [0 .. 5] do yield [|for i in [0 .. 39] do "."|]]

let drawScreen (input:string[] list) =
    input 
    |> Seq.map (fun l -> String.Join("", l))
    |> Seq.iter (printfn "%s")

let drawPixels ((screen:string [] list),crtPos,cpuPos) currentSignalChange =
    let spritePos = [cpuPos-1;cpuPos;cpuPos+1]
    if spritePos.Contains(crtPos % 40) then 
        let yPos = crtPos/40
        let xPos = crtPos % 40
        screen[yPos][xPos] <- "#"
    let newCpuPos = (changeSignalStrength (cpuPos,crtPos,[]) currentSignalChange) |> (fun (cp,_,_) -> cp)
    (screen, (crtPos+1), newCpuPos)

let part1 input =
    input 
    |> Seq.collect parseInput
    |> Seq.fold changeSignalStrength (1,1,[])
    |> fun (_,_,cp) -> cp |> Seq.sum

let part2 input =
    input 
    |> Seq.collect parseInput
    |> Seq.fold drawPixels (emptyScreen(),0,1)
    |> (fun (s,_,_) -> s)

let executeDay (testInput:string[]) (input:string[]) =
    // part 1
    testInput
    |> part1
    |> printfn "Part 1 Test: %d"

    input
    |> part1
    |> printfn "Part 1: %d"

    // part 2
    testInput
    |> part2
    |> drawScreen

    input
    |> part2
    |> drawScreen

