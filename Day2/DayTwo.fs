module DayTwo
open System.IO
open System.Text.RegularExpressions
    
// Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?
let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let readInputData = readLines(@"D:\Useful\AdventToCode2023\Day2\InputData.txt")

let findFirstNumber (inputString: string) =
    let matchResult = Regex.Match(inputString, @"\d+")
    match matchResult with
    | m when m.Success -> Some(int m.Value)
    | _ -> None

type CubeSet(text: string) =
    let mutable g = 0
    let mutable r = 0
    let mutable b = 0
    do 
        let cubes = text.Split ','
        for cubeText in cubes do 
            match cubeText with
            | c when c.Contains "green" -> g <- (findFirstNumber cubeText).Value
            | c when c.Contains "red" -> r <- (findFirstNumber cubeText).Value
            | c when c.Contains "blue" -> b <- (findFirstNumber cubeText).Value
            | c -> raise <| System.NotImplementedException($"Found {c} but expeced green, red, or blue")

    member this.GCube with get() = g and set(value) = g <- value
    member this.RCube with get() = r and set(value) = r <- value
    member this.BCube with get() = b and set(value) = b <- value

    new(green:int, red: int, blue: int) = CubeSet($"{blue} blue, {green} green, {red} red")

type CubeGame(text: string) =
    let mutable id = (findFirstNumber text).Value
    let mutable game : list<CubeSet> = []
    do
        let cubeSetsText =
            text.IndexOf ":" + 2
            |> text.Substring 
            |> fun x -> x.Split ';'
    
        for cubeSetText in cubeSetsText do 
            let cubeSet = CubeSet(cubeSetText)
            game <- List.append game [cubeSet]

    member this.Id with get() = id and set(value) = id <- value
    member this.Game with get() = game and set(value) = game <- value

    member this.TotalCubes() =
        let mutable totalGreen = 0
        let mutable totalRed = 0
        let mutable totalBlue = 0

        for cubeSet in this.Game do
            totalGreen <- totalGreen + cubeSet.GCube
            totalRed <- totalRed + cubeSet.RCube
            totalBlue <- totalBlue + cubeSet.BCube
        
        CubeSet(totalGreen, totalRed, totalBlue)

    member this.MaxCubes() =
        let mutable maxGreen = 0
        let mutable maxRed = 0
        let mutable maxBlue = 0

        for cubeSet in this.Game do
            if cubeSet.BCube > maxBlue then
                maxBlue <- cubeSet.BCube
            if cubeSet.RCube > maxRed then
                maxRed <- cubeSet.RCube
            if cubeSet.GCube > maxGreen then
                maxGreen <- cubeSet.GCube
        
        CubeSet(maxGreen, maxRed, maxBlue)

    member this.TotalCount() =
        let cubes = this.TotalCubes()
        cubes.GCube + cubes.RCube + cubes.BCube

//Game 1: 7 blue, 5 red; 10 red, 7 blue; 5 blue, 4 green, 15 red; 4 green, 6 red, 7 blue; 5 green, 8 blue, 4 red; 5 red, 4 blue, 3 green

// Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. What is the sum of the IDs of those games?
let play = 
    let mutable games: list<CubeGame> = []
    for gameText in readInputData do 
        let game = CubeGame(gameText)
        games <- List.append games [game]

    printfn $"{games.Length}"

    let maxRed = 12
    let maxGreen = 13
    let maxBlue = 14

    // let goodGamesSumId =
    //     games
    //     |> List.filter (fun x -> 
    //         let cubes = x.TotalCubes()
    //         printfn $"{x.Id} >> {cubes.BCube} <= {maxBlue} {cubes.BCube <= maxBlue} || {cubes.GCube} <= {maxGreen} {cubes.GCube <= maxGreen} || {cubes.RCube} <= {maxRed} {cubes.RCube <= maxRed}"
    //         cubes.BCube <= maxBlue && cubes.GCube <= maxGreen && cubes.RCube <= maxRed)
    //     |> List.map (fun x -> printf $"{x.Id},"; x.Id)
    //     |> List.sum

    let goodGamesSumId =
        let mutable total = 0
        for game in games do
            let cubes = game.TotalCubes()
            if cubes.BCube <= maxBlue && cubes.GCube <= maxGreen && cubes.RCube <= maxRed then
                total <- total + game.Id
        total

    let sumId2 = 
        let mutable total = 0
        for game in games do
            let cubes = game.MaxCubes()
            if cubes.BCube <= maxBlue && cubes.GCube <= maxGreen && cubes.RCube <= maxRed then
                total <- total + game.Id
        total

    let sumPower = 
        let mutable total = 0
        for game in games do
            let cubes = game.MaxCubes()
            let power = cubes.BCube * cubes.GCube * cubes.RCube
            total <- total + power
        total

    printfn $"{goodGamesSumId}"
    printfn $"{sumId2}"
    printfn $"{sumPower}"
    
