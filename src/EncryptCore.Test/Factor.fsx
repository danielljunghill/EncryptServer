    //[ 
    //    [ ;  ;]
    //    [  ;  ;  ]
    //    [  ;  ]
    //    [  ]
    //    [  ; ; 31; 31; 31]
    //    [  ; 57; ]
    //]




let factors =
    [ 
        [ 2 ; 2 ; 2 ; 2 ; 2 ; 2 ; 2]
        [ 3 ; 3 ; 3 ]
        [ 11 ; 11 ]
        [ 23 ]
        [ 31 ; 31 ; 31 ; 31 ; 31 ; 31]
        [ 57 ; 57 ; 57 ]
    ]

let a = 31 * 57
let b = 57 * 57 * 31 * 31

type NrOfItemsInPermutation = private | NrOfItemsInPermutation of int
module NrOfItemsInPermutation =
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

let count = NrOfItemsInPermutation.fromPrevFactor 1234 factors

let getMaxSizedList count (list: 'a list)  =
    if list.Length < count then list
    else list |> List.take count 

//gets list of factors 
let getListOfFactors count  =
     List.map (getMaxSizedList count)
     >> List.concat
     >> Seq.toList

// 
let countToList count = 
    seq { for i = 1 to count do yield i }
    |> Seq.toList

let test = countToList 5 |> List.toArray



type MaxNumberOfFactors = private | MaxNumberOfFactors of int
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
    >> Seq.toList

let permutations = 
    getPermutations (NrOfItemsInPermutation.create 3) (MaxNumberOfFactors.create 7) 
    |> Seq.toList

let getMinFactorForPermutations prevFactor (factors: int[]) permutations   =
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
    getMinimalFactor permutations

let calculateMinFactors factors prevFactor nrOfItemsInPermutation  =
    let nrOfItems = NrOfItemsInPermutation.value nrOfItemsInPermutation
    let factorList = getListOfFactors nrOfItems factors
    let nrOfFactors = factorList.Length
    let permutations = getPermutations nrOfItemsInPermutation (MaxNumberOfFactors.create nrOfFactors) 
    getMinFactorForPermutations prevFactor (factorList |> List.toArray) permutations

let minFactor,factorsInMinFactor = calculateMinFactors factors 123 (NrOfItemsInPermutation.create 2)

let removeFactors factors   =
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


let newFactors = removeFactors factors factorsInMinFactor

let getFactors prevFactor factors =
    let nrOfFactors = NrOfItemsInPermutation.fromPrevFactor prevFactor factors
    let rec calculateFactors factorCount =
        let minfactor = calculateMinFactors factors prevFactor (NrOfItemsInPermutation.create factorCount)
        let state = factorCount - 1
        if state <  0 then
            [ ]
        else 
            let min,_ = minfactor
            if min = 0 then
                calculateFactors (factorCount - 1)
            else
                minfactor ::  calculateFactors (factorCount - 1)
    let result = calculateFactors (NrOfItemsInPermutation.value nrOfFactors)
    if result.Length = 0 then
        printfn " factors %A" factors
        0, factors |> List.concat |> Seq.toList
    else
        result |> List.minBy (fun (factor,_) -> factor)



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

let getAllFactors  =
    let rec getAllFactors' prevFactor factors =
        let (factor, factorList) = getFactors prevFactor factors
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

       

//let getMinFactor prevFactor  nrOfItemsInPermutation maxNumberOfFactors factors =
//    let factorList = getListOfFactors 
//    getPermutations nrOfItemsInPermutation maxNumberOfFactors
//    |> Seq.toList
//    |> getMinFactorForPermutations prevFactor factors

let t = 54777  / (57 * 57 )


//let getMinFactors prevFactor factors =
//    let maxFactorsCount = calculcateMaximalNumbersOfFactors prevFactor factors
//    let rec getAllFactors index =

        
    
     
//börja med att räkna ut maximalt antal faktorer
//let rec getFactorsSequence lastFactor factors = 

//räkna ut lägsta faktor som är större än föregående

//räkna ut 