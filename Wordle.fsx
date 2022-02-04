open System
open System.Net.Http
open System.Collections.Generic

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

let getPositionedCharCount (words: string []) =
    [0..4]
    |> List.map (fun i -> 
        words 
        |> Array.map (fun word -> word[i]) |> String
        |> countByChar
        |> dict)

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

let wordPositionedValue (positionedCharCounts:IDictionary<char,int> list) (word:string) = 
    word :> seq<char>
    |> Seq.zip positionedCharCounts
    |> Seq.map (fun (cc, c) -> cc[c])
    |> Seq.sum

let findBestWord (words:string[]) = 
    words
    |> findWordValues
    |> Array.head
    |> fst

let findWorstWords (words:string[]) = 
    let l = if words.Length < 10 then 0 else words.Length - 10
    words
    |> findWordValues
    |> (fun a -> a.[l..])
    |> Array.map fst

let findWordBaseValues (baseWords:string[]) (words:string[]) = 
    let baseCharCounts = baseWords |> getCharCount
    let positionedCharCount = words |> getPositionedCharCount
    let charCounts = words |> getCharCount
    words
    |> Array.map (fun word -> 
        let positionedValue = word |> wordPositionedValue positionedCharCount
        let baseValue = word |> wordValue baseCharCounts
        let value = word |> wordValue charCounts
        word, positionedValue, value, baseValue)
    |> Array.sortByDescending (fun (word, v1, v2, v3) -> v1, v2, v3)

let findBestWord2 (baseWords:string[]) (words:string[]) = 
    let count = words.Length
    let wbv = words |> findWordBaseValues baseWords
    let av1 = wbv |> Array.averageBy (fun (_, v1, _, _) -> v1 |> float)
    let av2 = wbv |> Array.averageBy (fun (_, _, v2, _) -> v2 |> float)
    let av3 = wbv |> Array.averageBy (fun (_, _, _, v3) -> v3 |> float)
    wbv 
    |> Array.map (fun (word, v1, v2, v3) -> 
        let xxx = float v1/av1 + float v2/av2 + float v3/av3
        word, (xxx, (v1, v2, v3)))
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
    |> findBestWord2 allWords


let possibleWords =
    allWords
    |> guess "arose" "YBBBY"
    |> guess "lated" "YYYYB"

let withValues =
    possibleWords
    |> findBestWord2 allWords


let withUniqueCharsWithValues =
    possibleWords
    |> Array.filter hasOnlyUniqueChars
    |> findWordBaseValues allWords
