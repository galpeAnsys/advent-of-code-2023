open System.IO
// The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently any number adjacent to a symbol, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)
//What is the sum of all of the part numbers in the engine schematic?

printfn $"{Input.data}"

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

let mutable data: list<string> = []
for i in readInputData do 
    data <- List.append data [i]

let width = data[0].Length
let height = data.Length
let ints = ['0';'1'; '2'; '3';'4'; '5'; '6';'7';'8'; '9';]

printfn $"width: {width} height: {height}"
let mutable xi = 0
let mutable yi = 0

let schematics = Array2D.create height width ""
let mutable symbolsPos: list<(int * int)> = []

for y in data do
    xi <- 0
    let mutable num: string = ""
    let mutable pos: list<(int * int)> = []
    for x in y do
        if List.contains x ints then
            num <- num + x.ToString()
            pos <- List.append pos [(xi, yi)]
        else
            if num <> "" then
                for (px, py) in pos do
                    schematics[py, px] <- num
            
            if x <> '.' then
                symbolsPos <- List.append symbolsPos [(xi,yi)]

            num <- ""
            pos <- []

        xi <- xi+1

    yi <- yi+1
    

printfn $"{symbolsPos.Length}"