// For more information see https://aka.ms/fsharp-console-apps

open System.IO
open System.Text.RegularExpressions

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

let readInputData = readLines(@"D:\Useful\AdventToCode2023\Day4\input.txt")

type Card(text:string) = 
    let numbers = 
        let i = (text.IndexOf ':') + 1 
        let is = text.Substring i 
        is.Split '|'

    let parseNumbers strNumbers = 
        let matches = Regex.Matches(strNumbers, @"\d+") 
        let mutable nums: list<int> = []

        for m in matches do 
            nums <- List.append nums [int m.Value]

        nums

    member val Id = int Regex.Match(text, @"Card +(\d+):").Groups[1].Value
    member val GoalNums = parseNumbers numbers[0]
    member val PlayedNums = parseNumbers numbers[1]

let countPoints = 
    let mutable pointsSum = 0
    for cardText in readInputData do 
        let card = Card(cardText)

        let mutable winningNums: list<int> = []
        for num in card.PlayedNums do 
            if List.contains num card.GoalNums then
                winningNums <- List.append winningNums [num]

        let points = pown 2 (winningNums.Length-1)
        pointsSum <- pointsSum + points
        // printfn $"%A{card.GoalNums} | %A{card.PlayedNums} | %A{winningNums} | {points} | {pointsSum}"

    pointsSum

printfn $"{countPoints.ToString()}"

let numOfCards = Seq.length readInputData
let cardInstances = Array.create numOfCards 1 
for cardText in readInputData do 
    let card = Card(cardText)

    let mutable winningNums: list<int> = []
    for num in card.PlayedNums do 
        if List.contains num card.GoalNums then
            winningNums <- List.append winningNums [num]

    for i in [0..winningNums.Length-1] do
        let currVal = cardInstances[card.Id-1]
        let nextVal = cardInstances[card.Id+i]
        cardInstances[card.Id+i] <- nextVal + currVal

    // printfn $"%i{card.Id} | %i{winningNums.Length} | %i{cardInstances[card.Id-1]}"

printfn $"{Array.sum cardInstances}"