module Day17

open Common
open System

module Set =
    let addRange setToAdd set =
        (set |> Set.toList)@(setToAdd |> Set.toList) |> Set
let rawRockShapes = """####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##"""

// let jets = (getInput 17).ToCharArray() |> Array.map string
let jets (jetsInput:string) = jetsInput.ToCharArray() |> Array.map string
let getJet (jets:array<'a>) indx = jets[indx%(jets |> Array.length)]
let jetsLength jets = jets |> Seq.length

let rockShapes = 
    rawRockShapes.Split(Environment.NewLine + "" + Environment.NewLine)
    |> Array.map (fun s -> 
        s.Split(Environment.NewLine)
        |> Seq.rev
        |> Seq.mapi (fun y r -> (r.ToCharArray() |> Seq.indexed |> Seq.filter(fun (_,e) -> e <> '.')) |> Seq.map(fun (x,e) -> (x,y)))
        |> Seq.collect id)
    |> Seq.map Set
    |> Seq.toList
let rockShapesLength = rockShapes |> Seq.length

type Chamber = {Width:int; RockCoords:Set<int*int>; TopY:int}

let initChamber =
    let width = 7
    let floor = [0 .. width - 1] |> Seq.map (fun x -> (x,-1)) |> Set
    {Width = 7; RockCoords = floor; TopY = -1}

let printChamber (chamber:Chamber) =
    // printfn "%A" chamber
    let topY = (chamber.RockCoords |> Seq.maxBy(fun (_,y) -> y) |> snd) + 2
    let topX = (chamber.RockCoords |> Seq.maxBy(fun (x,_) -> x) |> fst) + 1
    let chamberMap = 
        Array2D.create topY topX "."
    chamber.RockCoords 
    |> Seq.iter (fun (x,y) -> 
        chamberMap[y+1,x] <- "#")

    [topY - 1 .. -1 ..  0] |> Seq.iter(fun y -> String.Join("", chamberMap[y,*]) |> printfn "%s")
    printfn "%d %d" topX topY

let canMoveLeft (chamber:Chamber) (fallingRockCoords:Set<int*int>) =
    (fallingRockCoords |> Seq.map fst |> Seq.min) > 0
     &&  fallingRockCoords |> Seq.forall (fun (x,y) -> not (chamber.RockCoords |> Seq.contains (x-1,y-1)))

let canMoveRight (chamber:Chamber) (fallingRockCoords:Set<int*int>) =
    (fallingRockCoords |> Seq.map fst |> Seq.max) < 6
     && fallingRockCoords |> Seq.forall (fun (x,y) -> not (chamber.RockCoords |> Seq.contains (x+1,y-1)))

let rec rockFallToHalt jets (chamber:Chamber) (fallingRockCoords:Set<int*int>) iter =
    // printChamber {chamber with RockCoords = chamber.RockCoords |> Set.addRange fallingRockCoords}
    let getJet = getJet jets
    let hasFallenToHalt = 
         fallingRockCoords |> Set.exists (fun (x,y) -> (chamber.RockCoords |> Set.contains (x,y-1)))

    // printfn "%A" (fallingRockCoords |> Set.exists (fun (x,y) -> (chamber.RockCoords |> Set.contains (x,y-1))))
    if hasFallenToHalt  then 
        let newRockCoords = chamber.RockCoords |> Set.addRange fallingRockCoords
        {chamber with RockCoords = newRockCoords; TopY = (newRockCoords |> Seq.maxBy snd |> snd)}, iter
    else 
        // fall down
        let fallingRockCoords' = 
            match (getJet iter) with
            | "<" when canMoveLeft chamber fallingRockCoords -> fallingRockCoords |> Set.map (fun (x,y) -> (x-1,y-1))
            | ">" when canMoveRight chamber fallingRockCoords -> fallingRockCoords |> Set.map (fun (x,y) -> (x+1,y-1))
            | _ -> fallingRockCoords |> Set.map (fun (x,y) -> (x,y-1))
        rockFallToHalt jets chamber fallingRockCoords' (iter+1)

let placeFallingRock chamber fallingRockInitialCoords =
    // let topY = chamber.RockCoords |> Seq.maxBy snd |> snd
    let topY = chamber.TopY
    // printfn "topY: %d" topY
    let xOffset = 2
    fallingRockInitialCoords |> Set.map (fun (x,y) -> (x+xOffset, y+topY+5))

let compressChamber chamber = 
    let minYCoord = 
        [0 .. 6] 
        |> Seq.map (fun i -> chamber.RockCoords |> Seq.filter(fun (x,_) -> x = i) |> Seq.maxBy snd |> snd)
        |> Seq.min
    {chamber with RockCoords = chamber.RockCoords |> Set.filter (fun (_,y) -> y >= minYCoord)}

let nextRound jets (chamber:Chamber,iter:int) shapeIndx =
    let fallingRockCoords = rockShapes[shapeIndx%(rockShapes |> Seq.length)] |> placeFallingRock chamber
    let (chamber', iter') = rockFallToHalt jets chamber fallingRockCoords iter
    (compressChamber chamber'), iter'
    // chamber', iter'

let simulate jets fallenRocksCount =
    let rounds = [0 .. fallenRocksCount-1]

    let nextRound = nextRound jets
    let jetsLength = jetsLength jets

    let chamber = initChamber
    rounds 
    |> Seq.fold nextRound (chamber,0)
    |> fun (c,i) -> 
        printfn "Total iter = %d pos: %d" i (i%jetsLength)
        c,i
    |> fst

let findRepeatPoint jets abortAfter =
    let chamber = initChamber
    let iter = 0

    // let rec repRound chamber iter rockIndx (detectedSets:list<(int*int*Set<int*int>)*int>) = 
    let rec repRound chamber iter rockIndx (detectedSets:list<(int*int*Set<int*int>*int)*int>) = 
        let normalizedRockIndx = (rockIndx % (rockShapesLength))
        let normalizedJetIndx = (iter % (jets |> Seq.length))
        if rockIndx > abortAfter then
            rockIndx
        else
            let topPattern = 
                [0 .. 6] 
                |> Seq.map (fun i -> chamber.RockCoords |> Seq.filter(fun (x,_) -> x = i) |> Seq.maxBy snd)
            // let lowY = chamber.RockCoords |> Seq.minBy snd |> snd
            let lowY = topPattern |> Seq.minBy snd |> snd
            let highY = topPattern |> Seq.minBy snd |> snd
            // let topPatternNormalized = chamber.RockCoords |> Seq.map (fun (x,y) -> (x,y-lowY)) |> Set
            let topPatternNormalized = topPattern |> Seq.map (fun (x,y) -> (x,y-lowY)) |> Set
            // let topPatternNormalized = topPattern |> Seq.map (fun (x,y) -> (x,y/2)) |> Set

            if highY % 2 = 0 && detectedSets |> Seq.map fst |> Seq.contains (normalizedRockIndx, normalizedJetIndx, topPatternNormalized,highY/2) then
                printfn "%d %d %d %A" rockIndx normalizedRockIndx normalizedJetIndx topPatternNormalized
                // let gna = detectedSets |> Seq.find (fun (ds,_) -> ds = ((normalizedRockIndx, normalizedJetIndx, topPatternNormalized,(highY/2))))
                // let ((_,_,_,s),_) = gna

                let (_,ri) = detectedSets |> Seq.find (fun (ds,_) -> ds = (normalizedRockIndx, normalizedJetIndx, topPatternNormalized,highY/2))
                ri
            else
                let (chamber',iter') = nextRound jets (chamber,iter) rockIndx
                let detectedSets' = 
                    ((normalizedRockIndx, normalizedJetIndx, topPatternNormalized, highY), rockIndx)::detectedSets
                    |> fun ds -> ds |> List.take (Math.Min(10000, ds |> Seq.length))
                repRound chamber' iter' (rockIndx + 1) detectedSets'

    repRound chamber iter 0 []


let part1 input =
    let parsedJets = jets input
    simulate parsedJets 2022
    |> fun c -> c.RockCoords |> Seq.maxBy snd |> snd
    |> (+) 1

let part2 input =
    input 
    |> Array.length

let executeDay day =
    // part 1
    getTestInputAllText day
    |> part1
    |> printfn "Part 1 Test: %d"

    getInputAllText day
    |> part1
    |> printfn "Part 1: %d"

    // part 2
    // getTestInputAllText day
    // |> part2
    // |> printfn "Part 2 Test: %d"

    // getInputAllText day
    // |> part2
    // |> printfn "Part 2: %d"
