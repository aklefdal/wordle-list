open System
open System.Net.Http

//
// String functions
//
let splitLines (s: string) = s.Split('\n')

let countByChar (s: string) =
    s
    |> Seq.countBy id
    |> Seq.sortByDescending snd
    |> Seq.toList

let uniqueChars (s: seq<char>) = s |> Seq.distinct |> String.Concat

let getCharCount (words: string []) =
    words
    |> Array.map uniqueChars
    |> String.Concat
    |> countByChar

let isWithoutChars (chars: seq<char>) (word: string) =
    chars
    |> Seq.filter (fun c -> word.Contains c)
    |> Seq.isEmpty

let hasOnlyUniqueChars (word: string) =
    (word |> uniqueChars) = word

//
// Functions for measuring value of a remaining possible word
//
let charValue (charCounts:(char*int) list) (c:char) = 
    charCounts
    |> List.find (fun (c2, _) -> c2 = c)
    |> snd

let wordValue (charCounts:(char*int) list) (s:string) = 
    s :> seq<char>
    |> Seq.distinct
    |> Seq.sumBy (fun c -> c |> charValue charCounts)

let findWordValues (words:string[]) = 
    let charCounts = words |> getCharCount
    words
    |> Array.map (fun word -> 
        let value = word |> wordValue charCounts
        word, value)
    |> Array.sortByDescending snd

let findBestWord (words:string[]) = 
    words
    |> findWordValues
    |> Array.head
    |> fst

let findWordBaseValues (baseWords:string[]) (words:string[]) = 
    let baseCharCounts = baseWords |> getCharCount
    let charCounts = words |> getCharCount
    words
    |> Array.map (fun word -> 
        let baseValue = word |> wordValue baseCharCounts
        let value = word |> wordValue charCounts
        word, (value, baseValue))
    |> Array.sortByDescending snd

//
// Functions and types for handling guesses
//
type Color =
    | Black
    | Yellow
    | Green
    | BlackWithAnother
    | YellowWithAnother

let toColor =
    function
    | 'B' -> Black
    | 'Y' -> Yellow
    | 'G' -> Green
    | c -> failwithf "Illegal value for color: %c" c

type Result = {
    Char: char
    Position: int
    Color: Color
}

let wordContainsCharTimes (c:char) (i:int) (word:string) : bool =
    let chars = word |> Seq.filter (fun c1 -> c1 = c) 
    Seq.length chars = i

let filterWords (words:string[]) (result: Result) =
    match result.Color with
    | Yellow -> words |> Array.filter (fun word -> word.Contains(result.Char) && word[result.Position] <> result.Char)
    | Green -> words |> Array.filter (fun word -> word[result.Position] = result.Char)
    | Black -> words |> Array.filter (fun word -> word.Contains(result.Char) |> not)
    | BlackWithAnother -> words |> Array.filter (fun word -> (word |> wordContainsCharTimes result.Char 1))
    | YellowWithAnother -> words |> Array.filter (fun word -> word[result.Position] <> result.Char && word |> wordContainsCharTimes result.Char 2)

let handleChar (resultsSoFar:Result list) (resultsForChar:Result list) = 
    match resultsForChar with
    | [] -> failwith "Ouch"
    | [result] -> result :: resultsSoFar
    | result1 :: result2 :: _ ->
        match result1.Color, result2.Color with
        | Green, Green
        | Black, Black -> result1 :: result2 :: resultsSoFar
        | Yellow, Yellow
        | Yellow, Green -> {result1 with Color = YellowWithAnother} :: result2 :: resultsSoFar
        | Green, Yellow -> result1 :: {result2 with Color = YellowWithAnother} :: resultsSoFar
        | Black, Green
        | Black, Yellow -> {result1 with Color = BlackWithAnother} :: result2 :: resultsSoFar
        | Green, Black
        | Yellow, Black -> result1 :: {result2 with Color = BlackWithAnother} :: resultsSoFar
        | BlackWithAnother, _
        | YellowWithAnother, _
        | _, BlackWithAnother
        | _, YellowWithAnother -> failwith "Boom"

handleChar [] [{Char = 'r'; Position = 2; Color = Yellow};{Char = 'r'; Position = 3; Color = Green}]

let handleDuplicateCharsInAnswer (answers:seq<Result>) =
    let byChar = answers |> Seq.toList |> List.groupBy (fun r -> r.Char) |> List.map snd
    byChar |> List.fold handleChar []

handleDuplicateCharsInAnswer 
    [
        {Char = 'm'; Position = 0; Color = Black}
        {Char = 'e'; Position = 1; Color = Green}
        {Char = 'r'; Position = 2; Color = Green}
        {Char = 'r'; Position = 3; Color = Black}
        {Char = 'y'; Position = 3; Color = Green} ]

let guess (word:string) (result:string) (words:string[]) =
    Seq.zip word result 
    |> Seq.mapi (fun pos (c, color) -> {Char = c; Position = pos; Color = color |> toColor })
    |> handleDuplicateCharsInAnswer
    |> Seq.fold filterWords words

//
// Download words
//

let getAllWords () =
    let url = "https://raw.githubusercontent.com/tabatkins/wordle-list/main/words"
    use httpClient = new HttpClient()
    httpClient.GetStringAsync(url).Result

let allWords = getAllWords () |> splitLines

//
// Let's guess!
//

let startWord = 
    allWords
    |> findBestWord

let secondWord =
    allWords
    |> guess startWord "BBBBB"
    |> findBestWord

let possibleWords =
    allWords
    |> guess "arose" "BYBBY"
    |> guess "tired" "BBGYB"
    |> guess "mercy" "BGGBG"
    |> guess "jerky" "BGGGG"

let withValues =
    possibleWords
    |> findWordBaseValues allWords


let withUniqueCharsWithValues =
    possibleWords
    |> Array.filter hasOnlyUniqueChars
    |> findWordBaseValues allWords
