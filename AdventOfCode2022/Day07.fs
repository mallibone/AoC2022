module Day07

open System

type NodeType = Directory | File
type FilesystemNode = { Name:string; Type:NodeType; Size:int; Parent:string }//Children:list<FilesystemNode>}

let (navigation:list<string>) = []

type FileSystemExplorer = {NavigationStack : list<string>; Nodes : list<FilesystemNode>}

let exploreFilesystem currentStructure nextCommand =
    let getPath navStack = String.Join("/", navStack |> List.rev)
    match nextCommand with
    | "$ ls" -> currentStructure
    | "$ cd .." -> {currentStructure with NavigationStack = currentStructure.NavigationStack.Tail}
    | nc when nc.StartsWith("$ cd ") -> 
        let directory = nc.Replace("$ cd ", "")
        {currentStructure with NavigationStack = directory::currentStructure.NavigationStack}
    | _ -> 
        match nextCommand with
        | nc when nc.StartsWith("dir ") ->
            let path = getPath currentStructure.NavigationStack
            let directory = path + "/" + nc.Replace("dir ", "")
            {currentStructure with Nodes = {Name = directory; Type = Directory; Size = 0; Parent = path}::currentStructure.Nodes }
        |_ -> 
            let path = getPath currentStructure.NavigationStack
            let size, name = (nextCommand.Split(" ") |> fun a -> a[0], a[1])
            let filePath = path + "/" + name
            {currentStructure with Nodes = {Name = filePath; Type = File; Size = int size; Parent = path}::currentStructure.Nodes }

let getFolderSizes fileStructure =
    let sumChildSizes nodes parentName =
        nodes |> Seq.filter (fun n -> n.Parent = parentName) |> Seq.sumBy (fun n -> n.Size)

    let updateDirectorySize nodes directoryNode =
        nodes |> List.map(fun n -> if n.Name = directoryNode.Name then {n with Size = (sumChildSizes nodes directoryNode.Name)} else n)

    fileStructure.Nodes 
    |> List.filter (fun n -> n.Type = Directory)
    |> List.sortByDescending (fun d -> d.Parent)
    |> List.fold updateDirectorySize fileStructure.Nodes
    |> List.filter (fun n -> n.Type = Directory)

let getSizeOfDirectoryToDeleteForUpdate directories =
    let usedSpace =
        (directories
        |> Seq.sortByDescending (fun d -> d.Size)
        |> Seq.head).Size

    let requiredFreeSpace = 30000000 - (70000000 - usedSpace)

    (directories
    |> Seq.filter (fun d -> d.Size >= requiredFreeSpace)
    |> Seq.sortBy(fun d -> d.Size)
    |> Seq.head).Size

let part1 input =
    input 
    |> Seq.fold exploreFilesystem {NavigationStack = []; Nodes = [{Name = "/"; Type = Directory; Size = 0; Parent = ""}]} 
    |> getFolderSizes
    |> Seq.filter (fun d -> d.Size <= 100000)
    |> Seq.sumBy (fun d -> d.Size)

let part2 input =
    input 
    |> Seq.fold exploreFilesystem {NavigationStack = []; Nodes = [{Name = "/"; Type = Directory; Size = 0; Parent = ""}]} 
    |> getFolderSizes
    |> getSizeOfDirectoryToDeleteForUpdate

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
    |> printfn "Part 2 Test: %d"

    input
    |> part2
    |> printfn "Part 2: %d"

