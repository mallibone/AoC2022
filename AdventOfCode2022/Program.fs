open System.Diagnostics

printfn "Advent of Code 2022"

let printDay day dayFunc =
    let sw = new Stopwatch()
    sw.Start()
    printfn $"**** Day %02d{day} ****"
    dayFunc day
    sw.Stop()
    printfn $"Duration {sw.ElapsedMilliseconds} ms"
    printfn $"****************"

[Day01.executeDay; Day02.executeDay; Day03.executeDay; Day04.executeDay; Day05.executeDay; Day06.executeDay; Day07.executeDay; 
    Day08.executeDay; Day09.executeDay; Day10.executeDay; Day11.executeDay; Day12.executeDay; Day13.executeDay; Day14.executeDay;
    Day15.executeDay; Day16.executeDay; Day17.executeDay; Day18.executeDay; ]
|> List.iteri (fun i func -> printDay (i+1) func)

// printDay 1 Day01.executeDay