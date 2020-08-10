let factors =
    [ 
        [ 2 ; 2 ; 2 ]
        [ 3 ; 3 ; 3 ]
        [ 11 ; 11 ]
        [ 23 ]
        [ 31 ]
    ]


let maxFactorsForLevel factors level =
    let factorList = List.concat factors
    let rec calculateNumberOfFactors (state: int) level facList  =
        match facList with
        | head :: tail ->
            let newState = state * head
            if newState > level then
                1
            else
                1 + calculateNumberOfFactors newState level tail 
        | [] -> 0
    calculateNumberOfFactors 1 level factorList
let count = maxFactorsForLevel factors 1234 

let countToList count = 
    seq { for i = 1 to count do yield i }
    |> Seq.toList

let test = countToList 5 |> List.toArray



let rec permutaion index count state (arr: int[]) =
    seq {
        //
        if count = 0 then
            yield state
        else
            for i = index to (arr.Length - count + 1) do
                let newState =  state @  [ arr.[i - 1] ]
                yield! permutaion (index + 1) (count - 1) newState arr
    }

let p = permutaion 1 3 List.empty test
    
     

let calculateMinFactor nr factors level =
    let rec calculateMinFactor' state factors =
        match factors with
        | head :: tail ->
            
        | [] ->
        

let calculateMinFactor factors level =
    let maxNrOfFactors = maxFactorsForLevel factors level
    



