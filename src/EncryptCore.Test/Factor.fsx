let factors =
    [ 
        [ 2 ; 2 ; 2 ]
        [ 3 ; 3 ; 3 ]
        [ 11 ; 11 ]
        [ 23 ]
        [ 31 ]
    ]


let calculcateMaximalNumbersOfFactors  level =
    let rec calculateNumberOfFactors (state: int) level facList  =
        match facList with
        | head :: tail ->
            let newState = state * head
            if newState > level then 1
            else 1 + calculateNumberOfFactors newState level tail 
        | [] -> 0
    List.concat
    >> calculateNumberOfFactors 1 level 

let count = calculcateMaximalNumbersOfFactors 1234 factors

let getMaxSizedList count (list: 'a list)  =
    if list.Length < count then list
    else list |> List.take count 


let calculateFactorsForCount count =
     List.map (getMaxSizedList count)
     >> List.concat
     >> Seq.toList
     

let countToList count = 
    seq { for i = 1 to count do yield i }
    |> Seq.toList

let test = countToList 5 |> List.toArray

type NrOfItemsInPermutation = | NrOfItemsInPermutation of int
module NrOfItemsInPermutation =
    let create = NrOfItemsInPermutation
    let value (NrOfItemsInPermutation nr) = nr

type MaxNumberOfFactors = | MaxNumberOfFactors of int
module MaxNumberOfFactors =
    let create = MaxNumberOfFactors
    let value (MaxNumberOfFactors nr) = nr


let getPermutaionsForList numberOfItemsInPermutation  =
    let rec permutaion startIndex nrOfItems state (arr: int[]) =
        seq {
            //
            if nrOfItems = 0 then
                yield state
            else
                for i = startIndex to (arr.Length - nrOfItems + 1) do
                    let newState =  state @  [ arr.[i - 1] ]
                    yield! permutaion (startIndex + 1) (nrOfItems - 1) newState arr
        }
    permutaion 1 (NrOfItemsInPermutation.value numberOfItemsInPermutation) List.empty 

let getPermutations nrOfItemsInPermutaion =
    MaxNumberOfFactors.value
    >> countToList
    >> List.toArray
    >> getPermutaionsForList nrOfItemsInPermutaion

 
let getMinFactorForPermutations prevFactor (factors: int[]) permutations   =
    let getValueForPermutation =
        List.fold (fun state index -> factors.[index - 1] * state) 1
    let rec getMinimalFactor permutations =
        match permutations with
        | head :: tail ->
            let factorValue = getValueForPermutation head 
            if factorValue > prevFactor then
                head,factorValue
            else
                getMinimalFactor tail
        | [] -> [],0
    getMinimalFactor permutations

let getMinFactor maxNumberOfFactors prevFactor factors nrOfItemsInPermutation    =
    getPermutations nrOfItemsInPermutation maxNumberOfFactors
    |> Seq.toList
    |> getMinFactorForPermutations prevFactor factors

let getMinFactors prevFactor factors =
    let maxFactorsCount = calculcateMaximalNumbersOfFactors prevFactor factors
    let rec getAllFactors index =

        
    
     
//börja med att räkna ut maximalt antal faktorer
let rec getFactorsSequence lastFactor factors = 

//räkna ut lägsta faktor som är större än föregående

//räkna ut 