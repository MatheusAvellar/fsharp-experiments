open System
Console.ForegroundColor <- ConsoleColor.Gray
Console.Title <- "Searching"
printf "################################################\n# Regular and Binary Search by Matheus Avellar #\n################################################\n\n\n"

module search =
    Console.ForegroundColor <- ConsoleColor.Cyan
    printf "  Regular Search\n------------------\n"
    Console.ForegroundColor <- ConsoleColor.White

    let regularArray = [|10 .. 100|]

    let regularSearch (arr:array<int>, item:int) = [| for i in 0 .. arr.Length-1 do if arr.[i] = item then yield i |]
    let regularItem = 51
    let regularResult = regularSearch(regularArray, regularItem).[0]

    printfn "Given %A\nSearched for %i\nItem %i is on position %A" regularArray regularItem regularItem regularResult

    Console.ForegroundColor <- ConsoleColor.Cyan
    printf "\n\n  Binary Search\n------------------\n"
    Console.ForegroundColor <- ConsoleColor.White

    let rec binarySearch (arr:array<int>, item:int, position:int) =
        let middle = arr.Length / 2
        if arr.Length > 0 then
            if arr.[middle] < item then
                binarySearch(arr.[middle+1..], item, position+middle+1)
            else if arr.[middle] > item then
                binarySearch(arr.[..middle-1], item, position)
            else if arr.[middle] = item then
                position+middle
            else
                0
        else
            0

    let binaryArray = [|10 .. 100|]
    let binaryItem = 51
    let binaryResult = binarySearch(binaryArray, binaryItem, 0)

    printfn "Given %A\nSearched for %i\nItem %i is on position %A" binaryArray binaryItem binaryItem binaryResult
Console.ReadKey() |> ignore