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

let filterWords (words:string[]) (result: Result) =
    match result.Color with
    | Yellow -> words |> Array.filter (fun word -> word.Contains(result.Char) && word[result.Position] <> result.Char)
    | Green -> words |> Array.filter (fun word -> word[result.Position] = result.Char)
    | Black -> words |> Array.filter (fun word -> word.Contains(result.Char) |> not)

let guess (word:string) (result:string) (words:string[]) =
    Seq.zip word result 
    |> Seq.mapi (fun pos (c, color) -> {Char = c; Position = pos; Color = color |> toColor })
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
    |> guess "arose" "BBYBB"
    |> guess "unity" "YYBYB"
    |> guess "count" "BGGGG"

let withValues =
    possibleWords
    |> findWordBaseValues allWords


let withUniqueCharsWithValues =
    possibleWords
    |> Array.filter hasOnlyUniqueChars
    |> findWordBaseValues allWords
