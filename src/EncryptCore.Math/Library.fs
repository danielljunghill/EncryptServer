namespace EncryptCore.Math



module Seq =
    let getIntegers startInt endInt =
        seq { for i = startInt to endInt do yield i }

type private NrOfItemsInPermutation = private | NrOfItemsInPermutation of int
module private NrOfItemsInPermutation =
    let create = NrOfItemsInPermutation
    let value (NrOfItemsInPermutation nr) = nr
    let fromPrevFactor prevFactor =
        let rec calculateNumberOfFactors (state: int) level facList  =
            match facList with
            | head :: tail ->
                let newState = state * head
                if newState > level then 1
                else 1 + calculateNumberOfFactors newState level tail 
            | [] -> 0
        List.concat
        >> calculateNumberOfFactors 1 prevFactor 
        >> NrOfItemsInPermutation

type private NrOfChoicesInPermutation = private | NrOfChoicesInPermutation of int
module private NrOfChoicesInPermutation =
    let create = NrOfChoicesInPermutation
    let value (NrOfChoicesInPermutation nr) = nr

type Permutations = private | Permutations of int list seq 
module Permutations =
    let toSeq (Permutations permutations) = permutations
    let toList = toSeq >> Seq.toList
    let create (NrOfItemsInPermutation nrOfItemsInPermutation) (NrOfChoicesInPermutation nrOfChoicesForPermutation) =
        let choices = Seq.getIntegers 1 nrOfChoicesForPermutation |> Seq.toArray
        let rec permutation' startIndex nrOfItems state (arr: int[]) =
                    seq {
                        if nrOfItems = 0 then
                            yield state
                        else
                            for i = startIndex to (arr.Length - nrOfItems + 1) do
                                let newState =  state @  [ arr.[i - 1] ]
                                yield! permutation' (startIndex + 1) (nrOfItems - 1) newState arr
                    }
        permutation' 1 nrOfItemsInPermutation List.empty choices
        |> Permutations

module List =
    let getListWithMaxLength maxLength (list: 'a list)  =
        if list.Length < maxLength then list
        else list |> List.take maxLength 


    let getListOfListWithMaxLength maxLength  =
        List.map (getListWithMaxLength maxLength)
        >> List.concat
        >> Seq.toList

    let multiply  =
        List.fold (fun state i -> state * i) 1


type CombinedFactor = private | CombinedFactor of int * int list
module CombinedFactor =
    let toInt (CombinedFactor (value,_)) = value
    let private getMin prevFactor (factors: int[])  =
        let getFactorValues = List.map (fun index -> factors.[index - 1])
        let getValueForPermutation =
            List.fold (fun state index -> factors.[index - 1] * state) 1
        let rec getMinimalFactor permutations =
            match permutations with
            | head :: tail ->
                let factorValue = getValueForPermutation head 
                if factorValue > prevFactor then
                    factorValue,getFactorValues head
                else
                    getMinimalFactor tail
            | [] -> 0,[]
        Permutations.toList
        >> getMinimalFactor 

    let calculateMinFactor factors prevFactor nrOfItemsInPermutation  =
        let nrOfItems = NrOfItemsInPermutation.value nrOfItemsInPermutation
        let factorList = List.getListOfListWithMaxLength nrOfItems factors
        let nrOfFactors = factorList.Length
        let permutations = Permutations.create nrOfItemsInPermutation (NrOfChoicesInPermutation nrOfFactors) 
        getMin prevFactor (factorList |> List.toArray) permutations
        |> CombinedFactor

    let removeFromFactors factors   =
        let removeFactorFromList factor factors =
            match factors with
            | head :: tail ->
                if head = factor then
                    true,tail
                else
                    false, factors
            | [] -> true, []
        let rec removeFactorFromLists factor factors =
            match factors with
            | head :: tail ->
                let removed, rest = removeFactorFromList factor head
                if removed then rest :: tail 
                else head :: removeFactorFromLists factor tail
            | [] -> []

        List.fold (fun state toRemove -> removeFactorFromLists toRemove state) factors
        >> List.filter (fun list -> list.Length > 0)

    let create prevFactor factors =
        let nrOfFactors = NrOfItemsInPermutation.fromPrevFactor prevFactor factors
        let rec calculateFactors factorCount =
            let minfactor = calculateMinFactor factors prevFactor (NrOfItemsInPermutation.create factorCount)
            let state = factorCount - 1
            if state <  0 then
                [ ]
            else 
                let (CombinedFactor (min,_)) = minfactor
                if min = 0 then
                    calculateFactors (factorCount - 1)
                else
                    minfactor ::  calculateFactors (factorCount - 1)
        let result = calculateFactors (NrOfItemsInPermutation.value nrOfFactors)
        if result.Length = 0 then
            let factorList = factors |> List.concat |> Seq.toList
            CombinedFactor (List.multiply factorList, factorList)
        else
            result |> List.minBy (fun factor -> toInt factor)
           
    let verifyLastFactor  =
        let veriftLastFactor' factors =
            let checkLastFactor lastFactor restfactors =
                match restfactors with
                | head :: tail -> 
                    if head >= lastFactor then head * lastFactor :: tail
                    else lastFactor :: restfactors        
                | [] ->  [ lastFactor ]
                        
            match factors with
            | head :: tail -> checkLastFactor head tail
            | [] -> []
        List.rev
        >> veriftLastFactor'
        >> List.rev




    let optimize  =
        let rec getAllFactors' prevFactor factors =
            let combinedFactor = create prevFactor factors
            if factor = 0 then
                printfn "%A" factorList
                [ factorList |> List.fold (fun state i -> state * i)  1 ]
            else
                let state = removeFactors factors factorList
                factor :: getAllFactors' factor  state
        getAllFactors' 1 
    >> verifyLastFactor

let allFactors = getAllFactors factors



verifyLastFactor [ 2 ; 3 ; 6; 7; 3 ]

        
            
        
        

let factors1 = getAllFactors factors
factors1.Length

   

//let getMinFactor prevFactor  nrOfItemsInPermutation maxNumberOfFactors factors =
//    let factorList = getListOfFactors 
//    getPermutations nrOfItemsInPermutation maxNumberOfFactors
//    |> Seq.toList
//    |> getMinFactorForPermutations prevFactor factors




//let getMinFactors prevFactor factors =
//    let maxFactorsCount = calculcateMaximalNumbersOfFactors prevFactor factors
//    let rec getAllFactors index =

    

 
//börja med att räkna ut maximalt antal faktorer
//let rec getFactorsSequence lastFactor factors = 

//räkna ut lägsta faktor som är större än föregående

//räkna ut 