open System


type Draw = {
    Blue: int;
    Green: int;
    Red: int;
}
type BagContents = Draw

let drawIsValid : (Draw * BagContents) -> bool = fun input -> (
    let draw = fst input
    let bagContents = snd input
    
    draw.Blue <= bagContents.Blue && draw.Red <= bagContents.Red && draw.Green <= bagContents.Green
)

let drawStringToDraw : string -> Draw = fun drawString -> (
    let cleanString = drawString.TrimStart().Replace(", ", ",")
    let mutable red = 0
    let mutable green = 0
    let mutable blue = 0
    
    cleanString.Split(",") |> Array.toSeq |> Seq.iter (fun drawColor -> 
        let amountOfcolor = drawColor.Split(" ")[0] |> Convert.ToInt32
        let color = drawColor.Split(" ")[1]
        if color = "blue" then blue <- amountOfcolor
        if color = "green" then green <- amountOfcolor
        if color = "red" then red <- amountOfcolor
    )
    {Blue = blue; Green = green; Red = red}
)

let part1 : string -> option<int> = fun input -> (
    let bagContent: BagContents = {Blue = 14; Green = 13; Red = 12}
    let gameID = (input.Split(":")[0]).Split(" ")[1] |> Convert.ToInt32;
    let draws = (input.Split(":")[1]).Split(";")
    let isValidGame = draws |> Array.toSeq |> Seq.forall (fun draw -> drawIsValid ((drawStringToDraw draw, bagContent)))
    
    if isValidGame then Some(gameID) else None
)

let part2 : string -> option<int> = fun input ->
    Some(-1)





//=================================TESTCASES==========================================

let testcasesPart1 = [
    ("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", Some(1)); 
    ("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", Some(2));
    ("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", None); 
    ("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", None);
    ("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", Some(5));
]

let finalResultPart1 = 8

let testcasesPart2 = [
    ("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", Some(1)); 
    ("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", Some(2));
    ("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", None); 
    ("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", None);
    ("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", Some(5));
]

let finalResultPart2 = -1 
//==========================VALIDATION+CMD_HANDLING===============================

let args : list<string> = fsi.CommandLineArgs |> Array.toList

type Part = 
    | Part1
    | Part2

let intToPart : int -> option<Part> = fun int -> 
    match int with
        | 1 -> Some(Part1)
        | 2 -> Some(Part2)
        | _ -> None

let part = args[1] |> Int32.Parse |> intToPart
let inputPath = if args.Length = 3 then Some(args[2]) else None

if inputPath.IsNone then
    match part with
        | Some(Part1) -> 
            let mutable sumOfResults = 0;
            testcasesPart1 |> List.iter (fun (testcase, expectedResult) -> 
                let result = (part1 testcase)
                sumOfResults <- sumOfResults + if result.IsSome then result.Value else 0;
                let outcome = if result = expectedResult then "Success!" else sprintf "Failed! Result: %O Expected: %O" result expectedResult
                printf "Testcase %s\n%s\n\n" testcase outcome)
            let outcome= if sumOfResults = finalResultPart1 then "SUCCESS!" else sprintf "FAILED! Result: %i Expected: %i" sumOfResults finalResultPart1
            printf "FINAL_RESULT\n%s" outcome
        | Some(Part2) -> 
            let mutable sumOfResults = 0;
            testcasesPart2 |> List.iter (fun (testcase, expectedResult) -> 
                let result = (part2 testcase)
                sumOfResults <- sumOfResults + if result.IsSome then result.Value else 0;
                let outcome = if result = expectedResult then "Success!" else sprintf "Failed! Result: %O Expected: %O" result  expectedResult.ToString
                printf "Testcase %s\n%s\n\n" testcase outcome)
            let outcome= if sumOfResults = finalResultPart2 then "SUCCESS!" else sprintf "FAILED! Result: %i Expected: %i" sumOfResults finalResultPart2
            printf "FINAL_RESULT\n%s" outcome
        | None -> failwith "Invalid part!"
else
    let input = System.IO.File.ReadAllLines inputPath.Value |> Array.toList
    match part with
        | Some(Part1) -> 
            let results = input |> List.map (fun line ->
                let result = part1 line
                if result.IsSome then result.Value else 0
            )
            let resultSum =  results |> List.sum
            printf "Part1 Result: %i" resultSum
        | Some(Part2) -> 
            let results = input |> List.map (fun line ->
                let result = part2 line
                if result.IsSome then result.Value else 0
            )
            let resultSum =  results |> List.sum
            printf "Part2 Result: %i" resultSum
        | None -> failwith "Invalid part!"