#time
#r "nuget: FSharp.Collections.ParallelSeq, 1.2.0"
open System.IO
open System.Text.RegularExpressions;
open System
open System.Linq
open System.Collections.Generic
open FSharp.Collections.ParallelSeq
open System.Collections.Concurrent

let getTestInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/TestDay{day}.txt")
    File.ReadAllLines(filename day)

let getInput day =
    let filename day = Path.Combine(__SOURCE_DIRECTORY__, $"Input/Day{day}.txt")
    File.ReadAllLines(filename day)

// part 1
// getInput 10
getTestInput 10

// part 2
// getInput 10
// getTestInput 10
