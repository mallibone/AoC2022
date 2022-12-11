module Day11

type Monkey = {Items:list<int64>; Operation:int64 -> int64; Test: int64 -> int; TestNumber : int64; InspectionCount: int64}

let parseMonkey (input:string[]) =
    let items = (input[0].Split(":")[1]).Trim().Split(", ") |> Seq.map int64 |> Seq.toList
    let operation = 
        if input[1].Contains("*") then
            let numberVal = (input[1].Split(" * ")[1])
            if numberVal = "old" then
                fun i -> i * i
            else
                fun i -> i * (int64 numberVal)
        else
            let numberVal = (input[1].Split(" + ")[1])
            if numberVal = "old" then
                fun i -> i + i
            else
                fun i -> i + (int64 numberVal)

    let testNumber = int64 (input[2].Split(" by ")[1])
    let test =
        let trueResult = int (input[3].Split(" to monkey ")[1])
        let falseResult = int (input[4].Split(" to monkey ")[1])
        fun i -> if i % testNumber = 0L then trueResult else falseResult
    {Items = items; Operation = operation; Test = test; TestNumber = testNumber; InspectionCount = 0L}


let parseInput (input:string) =
    input.Split(System.Environment.NewLine + "" + System.Environment.NewLine)
    |> Array.map (fun monkeyInput -> monkeyInput.Split(System.Environment.NewLine) |> Array.skip 1 |> parseMonkey)
    |> Array.toList

let playRound reducer monkeyStates roundIdx =
    let monkeyIdx = roundIdx % (monkeyStates |> List.length)
    let currentMonkey = monkeyStates[monkeyIdx]

    if (currentMonkey.Items |> List.isEmpty) then 
        monkeyStates
    else
        let processItem monkeys currentItem =
            let itemToThrow = 
                currentItem
                |> currentMonkey.Operation
                |> reducer
            let targetMonkey =
                currentMonkey.Test itemToThrow

            monkeys 
            |> List.mapi (fun i m ->
                match i with
                | i when i = monkeyIdx -> {m with Items = m.Items |> List.tail; InspectionCount = m.InspectionCount + 1L}
                | i when i = targetMonkey -> {m with Items = m.Items@[itemToThrow]}
                | _ -> m)

        currentMonkey.Items
        |> List.fold processItem monkeyStates

let playRounds reducer rounds (monkeyStates:list<Monkey>) =
    [0 .. rounds * (monkeyStates |> List.length) - 1]
    |> Seq.fold (playRound reducer) monkeyStates

let reducerFunctionPartII monkeys =
    let ggt = monkeys |> Seq.map (fun m -> m.TestNumber) |> Seq.reduce (*)
    fun n -> n % ggt

let part1 input =
    input 
    |> parseInput
    |> playRounds (fun n -> n/3L) 20
    |> List.map (fun m -> m.InspectionCount)
    |> List.sortDescending |> List.take 2 |> List.reduce (*)

let part2 input =
    input 
    |> parseInput
    |> fun monkeys -> playRounds (reducerFunctionPartII monkeys) 10000 monkeys
    |> List.map (fun m -> m.InspectionCount)
    |> List.sortDescending |> List.take 2 |> List.reduce (*)

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

