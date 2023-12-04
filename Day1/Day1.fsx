open System

let testcasesPart1 = [
    ("1abc2", "12"); 
    ("pqr3stu8vwx", "38");
    ("a1b2c3d4e5f", "15"); 
    ("treb7uchet", "77")
]

let finalResultPart1 = 142

let testcasesPart2 = [
    ("two1nine", "29"); 
    ("eightwothree", "83");
    ("abcone2threexyz", "13"); 
    ("xtwone3four", "24");
    ("4nineeightseven2", "42");
    ("zoneight234", "14");
    ("7pqrstsixteen", "76")
]

let finalResultPart2 = 281

let isNumberFromChar : char -> bool = fun c -> (
    c |> Char.IsNumber
)

// let part1 : string -> string = fun input -> (
//     let firstDigit = input |> Seq.find (fun character -> (isNumber character))
//     let lastDigit = input |> Seq.findBack (fun character -> (isNumber character))
//     Char.ToString firstDigit + Char.ToString lastDigit
// )

let part1 : string -> string = fun input -> (
    let firstDigit = input |> Seq.filter (fun character -> (isNumberFromChar character)) |> Seq.head
    let lastDigit = input |> Seq.rev |> Seq.filter(fun character -> (isNumberFromChar character)) |> Seq.head
    Char.ToString firstDigit + Char.ToString lastDigit
)

let numbersAscendingByLenght = ["1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"; "one"; "two"; "six"; "four"; "five"; "nine"; "three"; "seven"; "eight"]
let stringToNumber : string -> option<int> = fun s ->
    let triplet = numbersAscendingByLenght |> List.map (fun x ->
        let index = s.IndexOf x
        if index = -1 then None else Some(x; index)
    )

    
    match  with
        | "1"
        | "one" -> Some(1)
        | "2"
        | "two" -> Some(2)
        | "3"
        | "three" -> Some(3)
        | "4"
        | "four" -> Some(4)
        | "5"
        | "five" -> Some(5)
        | "6"
        | "six" -> Some(6)
        | "7"
        | "seven" -> Some(7)
        | "8"
        | "eight" -> Some(8)
        | "9"
        | "nine" -> Some(9)
        | _ -> None

let part2 : string -> string = fun input ->
    let firstDigit = input |> Seq.windowed 5 |> Seq.map (fun slice -> slice |> String |> stringToNumber) |> Seq.find (fun x -> not x.IsNone)  |> Convert.ToString
    let lastDigit = input |> Seq.windowed 5 |> Seq.rev |> Seq.map (fun slice -> slice |> String |> stringToNumber) |> Seq.find (fun x -> not x.IsNone) |> Convert.ToString
    printf "%s" firstDigit
    printf "%s" lastDigit
    "5"





//===========================================================================
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
                sumOfResults <- sumOfResults + (result |> Int32.Parse);
                let outcome = if result = expectedResult then "Success!" else sprintf "Failed! Result: %s Expected: %s" result expectedResult
                printf "Testcase %s\n%s\n\n" testcase outcome)
            let outcome= if sumOfResults = finalResultPart1 then "SUCCESS!" else sprintf "FAILED! Result: %i Expected: %i" sumOfResults finalResultPart1
            printf "FINAL_RESULT\n%s" outcome
        | Some(Part2) -> 
            let mutable sumOfResults = 0;
            testcasesPart2 |> List.iter (fun (testcase, expectedResult) -> 
                let result = (part2 testcase)
                sumOfResults <- sumOfResults + (result |> Int32.Parse);
                let outcome = if result = expectedResult then "Success!" else sprintf "Failed! Result: %s Expected: %s" result expectedResult
                printf "Testcase %s\n%s\n\n" testcase outcome)
            let outcome= if sumOfResults = finalResultPart2 then "SUCCESS!" else sprintf "FAILED! Result: %i Expected: %i" sumOfResults finalResultPart2
            printf "FINAL_RESULT\n%s" outcome
        | None -> failwith "Invalid part!"
else
    let input = System.IO.File.ReadAllLines inputPath.Value |> Array.toList
    match part with
        | Some(Part1) -> 
            let results = input |> List.map (fun line ->
                part1 line |> Int32.Parse
            )
            let resultSum =  results |> List.sum
            printf "Part1 Result: %i" resultSum
        | Some(Part2) -> 
            let results = input |> List.map (fun line ->
                part1 line |> Int32.Parse
            )
            let resultSum =  results |> List.sum
            printf "Part2 Result: %i" resultSum
        | None -> failwith "Invalid part!"