open System.IO
open System.Diagnostics

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

printfn "Advent of Code 2022"

let printDay day dayFunc =
    let sw = new Stopwatch()
    sw.Start()
    printfn $"**** Day {day} ****"
    dayFunc (getTestInput day) (getInput day)
    sw.Stop()
    printfn $"Duration {sw.ElapsedMilliseconds} ms"
    printfn $"***************"

[Day01.executeDay; Day02.executeDay; Day03.executeDay; Day04.executeDay; Day05.executeDay; Day06.executeDay; Day07.executeDay; 
    Day08.executeDay]
|> List.iteri (fun i func -> printDay (i+1) func)

// printDay 1 Day01.executeDay