open System.IO
open System.Text.RegularExpressions

let numberString = 
    [1, "one"; 2, "two"; 3, "three"; 4, "four"; 5, "five"; 6, "six"; 7, "seven"; 8, "eight"; 9, "nine";]

let readLines (filePath:string) = seq {
    use sr = new StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}

// two1nine
// eightwothree
// eightwone --> 8, 2, 1
// eightwoneight --> 8, 2, 1, 8
// eight

// abcone2threexyz
// xtwone3four
// 4nineeightseven2
// zoneight234
// 7pqrstsixteen

type StrMatch(index: int, intNum: int, strNum: string) =
    member val Index = index
    member val IntNum = intNum
    member val StrNum = strNum

    new() = StrMatch(-1, -1, "")

let firstAndLastNumber(inputStr: string) =
    let matches = Regex.Matches(inputStr, @"\d")
    let mutable firstIntMatch: Match = Match.Empty
    let mutable lastIntMatch: Match = Match.Empty
    if matches.Count > 0 then
        firstIntMatch <- matches.Item 0
        lastIntMatch <- matches.Item (matches.Count-1)

    let mutable firstStrIndexes = []
    let mutable lastStrIndexes = []
    for (numInt, numStr) in numberString do
        let iFirst = inputStr.IndexOf numStr
        let iLast = inputStr.LastIndexOf numStr
        if iFirst <> -1 then
            firstStrIndexes <- List.append firstStrIndexes [StrMatch(iFirst, numInt, numStr)]
        if iLast <> -1 then
            lastStrIndexes <- List.append lastStrIndexes [StrMatch(iLast, numInt, numStr)]

    let mutable firstStrMatch: StrMatch = StrMatch()
    let mutable lastStrMatch: StrMatch = StrMatch()
    if firstStrIndexes.Length > 0 then
        let sortedByIndex = firstStrIndexes |> List.sortBy (fun (i) -> i.Index)
        firstStrMatch <- sortedByIndex.Head

    if lastStrIndexes.Length > 0 then
        let sortedByIndex = lastStrIndexes |> List.sortByDescending (fun (i) -> i.Index)
        lastStrMatch <- sortedByIndex.Head
    let mutable firstRes = 0
    let mutable lastRes = 0
    if firstStrMatch.Index = -1 || firstIntMatch.Index < firstStrMatch.Index then
        firstRes <- int firstIntMatch.Value
    else
        firstRes <- firstStrMatch.IntNum

    if lastIntMatch.Index > lastStrMatch.Index then
        lastRes <- int lastIntMatch.Value
    else
        lastRes <- lastStrMatch.IntNum

    (firstRes, lastRes)

let converStringNumbersToInt (inputString: string) =
    let mutable replacedString  = inputString
    let mutable go = true
    while go do
        let mutable indexes = []
        for (numInt, numStr) in numberString do
            let i = replacedString.IndexOf numStr
            if i <> -1 then
                indexes <- List.append indexes [(i, (numInt, numStr))]

        if indexes.Length > 0 then
            let sortedByIndex = indexes |> List.sortBy (fun (i, _) -> i)
            let (_, intStr) = sortedByIndex.Head
            let (numInt, numStr) = intStr

            replacedString <- replacedString.Replace (numStr, numInt.ToString())
        else
            go <- false

    replacedString

let findFirstNumber (inputString: string) =
    let matchResult = Regex.Match(inputString, @"\d")
    match matchResult with
    | m when m.Success -> Some(int m.Value)
    | _ -> None
    
let findCalibrationValue (input: string) =
        let convertedInput = converStringNumbersToInt input
        let firstNumber = findFirstNumber convertedInput
        let reveseredChars = convertedInput.ToCharArray() |> Array.rev
        let reversedText = System.String(reveseredChars)
        let lastNumber = findFirstNumber reversedText
        let joinedVal = int(firstNumber.Value.ToString() + lastNumber.Value.ToString())
        // printfn $"{input} || {convertedInput} : {firstNumber.Value}, {lastNumber.Value}, {joinedVal}"
        joinedVal

let findCalibrationValue2 (input:string) = 
    let (first, last) = firstAndLastNumber(input)
    let joinedVal = int(first.ToString() + last.ToString())
    joinedVal

let totalSum (path: string) = 
    let mutable sum = 0
    for textLine in readLines(path) do
        let calibrationValue = findCalibrationValue textLine
        sum <- sum + calibrationValue
    sum

let totalSum2 (path: string) = 
    let mutable sum = 0
    for textLine in readLines(path) do
        let calibrationValue = findCalibrationValue2 textLine
        sum <- sum + calibrationValue
    sum

let totalSum3 (path: string) = 
    let mutable sum1 = 0
    let mutable sum2 = 0
    for textLine in readLines(path) do
        let calibrationValue1 = findCalibrationValue textLine
        sum1 <- sum1 + calibrationValue1
        let calibrationValue2 = findCalibrationValue2 textLine
        sum2 <- sum2 + calibrationValue2
        if calibrationValue1 <> calibrationValue2 then
            printfn $"{calibrationValue1} || {calibrationValue2} : {textLine}"

    printfn $"{sum1} || {sum2}"
    

let largeImputRes = totalSum @"D:\Useful\AdventToCode2023\MyFSharpApp\input.txt"
printfn $"{largeImputRes}"
let largeImputRes2 = totalSum2 @"D:\Useful\AdventToCode2023\MyFSharpApp\input.txt"
printfn $"{largeImputRes2}"
let simpleImputRes = totalSum @"D:\Useful\AdventToCode2023\MyFSharpApp\simpleInput.txt"
printfn $"{simpleImputRes}"
let simpleImputRes2 = totalSum2 @"D:\Useful\AdventToCode2023\MyFSharpApp\simpleInput.txt"
printfn $"{simpleImputRes2}"

totalSum3 @"D:\Useful\AdventToCode2023\MyFSharpApp\input.txt"
totalSum3 @"D:\Useful\AdventToCode2023\MyFSharpApp\simpleInput.txt"

let a1 = findCalibrationValue2 "eight72threefivefour9onefive"
let a2 = findCalibrationValue2 "6twotwo18eightthreeeight"
printfn $"{a1.ToString()}"
printfn $"{a2.ToString()}"


