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
    [ 0..4 ]
    |> List.map (fun i ->
        words
        |> Array.map (fun word -> word[i])
        |> String
        |> countByChar
        |> dict)

let isWithoutChars (chars: seq<char>) (word: string) =
    chars
    |> Seq.filter (fun c -> word.Contains c)
    |> Seq.isEmpty

let hasOnlyUniqueChars (word: string) = (word |> uniqueChars) = word


//
// Functions for measuring value of a remaining possible word
//
let charValue (charCounts: (char * int) list) (c: char) =
    charCounts
    |> List.find (fun (c2, _) -> c2 = c)
    |> snd

let wordValue (charCounts: (char * int) list) (s: string) =
    s :> seq<char>
    |> Seq.distinct
    |> Seq.sumBy (fun c -> c |> charValue charCounts)

let findWordValues (words: string []) =
    let charCounts = words |> getCharCount

    words
    |> Array.map (fun word ->
        let value = word |> wordValue charCounts
        word, value)
    |> Array.sortByDescending snd

let wordPositionedValue (positionedCharCounts: IDictionary<char, int> list) (word: string) =
    word :> seq<char>
    |> Seq.zip positionedCharCounts
    |> Seq.map (fun (cc, c) -> cc[c])
    |> Seq.sum

type WordValues =
    { Word: string
      BaseValue: int // Based upon how often the characters in the word occur in all words in the set
      PositionedValue: int // Based upon how often the characters in the word occur in the right position in all words in the set
    // RemainingValue: int// Based upon how often the characters in the word occur in the **remaining** words in the set
     }

let findWordBaseValues (baseWords: string []) (words: string []) =
    let baseCharCounts = baseWords |> getCharCount
    let positionedCharCount = words |> getPositionedCharCount
    // let charCounts = words |> getCharCount
    words
    |> Array.map (fun word ->
        { Word = word
          PositionedValue = word |> wordPositionedValue positionedCharCount
          BaseValue = word |> wordValue baseCharCounts
        //   RemainingValue = word |> wordValue charCounts
        })

let findBestWords (baseWords: string []) (words: string []) =
    let wordBaseValues = words |> findWordBaseValues baseWords

    let averagePositionedValue =
        wordBaseValues
        |> Array.averageBy (fun wordValues -> wordValues.PositionedValue |> float)

    let averageBaseValue =
        wordBaseValues
        |> Array.averageBy (fun wordValues -> wordValues.BaseValue |> float)
    // let averageRemainingValue = wordBaseValues |> Array.averageBy (fun wordValues -> wordValues.RemainingValue |> float)

    wordBaseValues
    |> Array.map (fun wordValues ->
        let score =
            (float wordValues.PositionedValue)
            / averagePositionedValue
            + (float wordValues.BaseValue) / averageBaseValue
        // + (float wordValues.RemainingValue)/averageRemainingValue
        wordValues.Word, (score, (wordValues.PositionedValue, wordValues.BaseValue)))
    |> Array.sortByDescending snd
    |> Array.truncate 15

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

type Result =
    { Char: char
      Position: int
      Color: Color }

let wordContainsCharTimes (c: char) (i: int) (word: string) : bool =
    let chars = word |> Seq.filter (fun c1 -> c1 = c)
    Seq.length chars = i

let filterWords (words: string []) (result: Result) =
    match result.Color with
    | Yellow ->
        words
        |> Array.filter (fun word ->
            word.Contains(result.Char)
            && word[result.Position] <> result.Char)
    | Green ->
        words
        |> Array.filter (fun word -> word[result.Position] = result.Char)
    | Black ->
        words
        |> Array.filter (fun word -> word.Contains(result.Char) |> not)
    | BlackWithAnother ->
        words
        |> Array.filter (fun word -> (word |> wordContainsCharTimes result.Char 1))
    | YellowWithAnother ->
        words
        |> Array.filter (fun word ->
            word[result.Position] <> result.Char
            && word |> wordContainsCharTimes result.Char 2)

let handleChar (resultsSoFar: Result list) (resultsForChar: Result list) =
    match resultsForChar with
    | [] -> failwith "Ouch"
    | [ result ] -> result :: resultsSoFar
    | [ result1; result2 ] ->
        match result1.Color, result2.Color with
        | Green, Green
        | Black, Black -> result1 :: result2 :: resultsSoFar
        | Yellow, Yellow
        | Yellow, Green ->
            { result1 with Color = YellowWithAnother }
            :: result2 :: resultsSoFar
        | Green, Yellow ->
            result1
            :: { result2 with Color = YellowWithAnother }
               :: resultsSoFar
        | Black, Green
        | Black, Yellow ->
            { result1 with Color = BlackWithAnother }
            :: result2 :: resultsSoFar
        | Green, Black
        | Yellow, Black ->
            result1
            :: { result2 with Color = BlackWithAnother }
               :: resultsSoFar
        | BlackWithAnother, _
        | YellowWithAnother, _
        | _, BlackWithAnother
        | _, YellowWithAnother -> failwith "Boom"
    | _ -> failwith "Three equal chars in a word is not supported (yet)"

let handleDuplicateCharsInAnswer (answers: seq<Result>) =
    let byChar =
        answers
        |> Seq.toList
        |> List.groupBy (fun r -> r.Char)
        |> List.map snd

    byChar |> List.fold handleChar []

let guess (word: string) (result: string) (words: string []) =
    Seq.zip word result
    |> Seq.mapi (fun pos (c, color) ->
        { Char = c
          Position = pos
          Color = color |> toColor })
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
    |> findBestWords allWords
    |> Array.head
    |> fst

let bestWords =
    allWords
    |> guess "tares" "BBBBB"
    |> guess "doily" "YBYBB"
    |> guess "cupid" "BBBGG"
    |> findBestWords allWords
