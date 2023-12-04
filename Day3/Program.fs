open System.IO
// The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)
//What is the sum of all of the part numbers in the engine schematic?

// map of all points
// number spans more than one pos
// find symbol (whats the full list of symbols? Anything not a . not a number?)
// check neightbor + diagonal of symbol

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let readInputData = readLines(@"D:\Useful\AdventToCode2023\Day3\day3Input.txt")
// let readInputData = readLines(@"D:\Useful\AdventToCode2023\Day3\simpleInput.txt")

let mutable data: list<string> = []
for i in readInputData do 
    data <- List.append data [i]

let width = data[0].Length
let height = data.Length
let ints = ['0';'1'; '2'; '3';'4'; '5'; '6';'7';'8'; '9';]

printfn $"width: {width} height: {height}"
let mutable xi = 0
let mutable yi = 0

let schematics = Array2D.create height width ("", 0)
let mutable symbolsPos: list<((int * int) * char)> = []

let mutable id: int = 0
for y in data do
    // if List.contains yi [11;12] then
    //     printfn $"{y}"
    xi <- 0
    let mutable num: string = ""
    let mutable pos: list<(int * int)> = []
    for x in y do
        if List.contains x ints then
            num <- num + x.ToString()
            pos <- List.append pos [(xi, yi)]
        else
            if num <> "" then
                id <- id + 1
                for (px, py) in pos do
                    schematics[py, px] <- (num, id)
            
            if x <> '.' then
                symbolsPos <- List.append symbolsPos [(xi,yi), x]

            num <- ""
            pos <- []

        xi <- xi+1
    // num at the end of the line
    if num <> "" then
        id <- id + 1
        for (px, py) in pos do
            schematics[py, px] <- (num, id)

    yi <- yi+1
    

// printfn $"%A{schematics}"

let getNeightbors (x: int, y: int) =
    // 8 possible neightbors
    // top = x, y+1
    let options = [
        (x, y+1); //S
        (x+1, y+1); // SE
        (x+1, y); // E
        (x+1, y-1); // NE
        (x, y-1); // N
        (x-1, y-1); // NW
        (x-1, y); // W
        (x-1, y+1); // SW
    ]
    let filteredOptions: (int * int) list = 
        options 
        |> List.filter (fun (x, y) -> x >= 0 && x <= width && y >= 0 && y <= height) 

    filteredOptions
    

let neighborNumbers (symbolPos: int * int) = 
    let neighborNumId =
        getNeightbors symbolPos
        |> List.map (fun (x,y) -> 
            match schematics[y, x] with
            | (num, id) when num <> "" -> Some(num, id)
            | _ -> None)
        |> List.filter (fun x -> x.IsSome)
        |> List.map (fun x -> x.Value)
        |> List.distinct
    // printfn $"%A{symbol} >> %A{neighborNumId}"

    neighborNumId
    |> List.map (fun (num, id) -> int num)

let sumOfNeighbors (symbol: (int * int) * char) =
    let (symbolPos, symbolChar) = symbol
    let neighborNumbers = neighborNumbers symbolPos
    let theSum = 
        neighborNumbers
        |> List.sum
    
    if theSum = 0 then
        printfn $"%A{symbol} >> %A{neighborNumbers} >> %i{theSum}"
    
    theSum

let gearRatios (symbolPos: int * int)  = 
    let neighborNumbers = neighborNumbers symbolPos

    match neighborNumbers.Length with
    | 2 -> Some(neighborNumbers[0] * neighborNumbers[1])
    | _ -> None

let totalSum = 
    let neighborSums =
        symbolsPos
        |> List.map (fun symbol -> sumOfNeighbors symbol)
    
    // printfn $"%A{neighborSums.Length} >> %A{neighborSums}"

    neighborSums
    |> List.sum

let sumOfGearRatios = 
    symbolsPos
    |> List.filter(fun (_, sysChar) -> sysChar = '*')
    |> List.map (fun (sysNum, _) -> gearRatios sysNum)
    |> List.filter (fun g -> g.IsSome)
    |> List.map (fun x -> x.Value)
    |> List.sum

printfn $"%A{schematics[12, 136]}"
printfn $"Number of symbols: {symbolsPos.Length}"
printfn $"totalSum: %i{totalSum}"
printfn $"sumOfGearRatios: %i{sumOfGearRatios}"