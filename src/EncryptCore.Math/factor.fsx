let factors =
    [ 
        [ 2 ; 2 ; 2 ; 2 ; 2 ; 2 ; 2]
        [ 3 ; 3 ; 3 ]
        [ 11 ; 11 ]
        [ 23 ]
        [ 31 ; 31 ; 31 ; 31 ; 31 ; 31]
        [ 57 ; 57 ; 57; 57; 57]
      
    ]

module Seq =
    let getIntegers startInt endInt =
        seq { for i = startInt to endInt do yield i }


type  PermutationSize =  private | PermutationSize of int
module private PermutationSize =
    let create = PermutationSize
    let value (PermutationSize nr) = nr
    let calculate prevFactor =
        let rec calculateNumberOfFactors (state: int) level facList  =
            match facList with
            | head :: tail ->
                let newState = state * head
                if newState > level then 1
                else 1 + calculateNumberOfFactors newState level tail 
            | [] -> 0
        List.concat
        >> calculateNumberOfFactors 1 prevFactor 
        >> PermutationSize

type  PermutationListSize =  private | PermutationListSize of int
module private PermutationListSize =
    let create = PermutationListSize
    let value (PermutationListSize size) = size

type Permutations = private | Permutations of int list seq 
module Permutations =
    let toSeq (Permutations permutations) = permutations
    let toList = toSeq >> Seq.toList
    let create (PermutationSize permutationSize) (PermutationListSize nrOfChoicesForPermutation) =
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
        permutation' 1 permutationSize List.empty choices
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


type Factor =  
    private | Factor of int list
            | Rest of int list
            

module Factor =
    let toIntList factor =
        match factor with
        | Factor ls -> ls
        | Rest ls -> ls

    let toInt = toIntList >> List.multiply

    let merge factor1 factor2 =
        let newList = toIntList factor1 @ toIntList factor2 |> List.sortBy (fun i -> i)
        Factor newList
   
    let private tryGetMin prevFactor (factors: int[])  =
        let getFactorValues = List.map (fun index -> factors.[index - 1])
        let getValueForPermutation =
            List.fold (fun state index -> factors.[index - 1] * state) 1
        let rec getMinimalFactor permutations =
            match permutations with
            | head :: tail ->
                let factorValue = getValueForPermutation head 
                if factorValue > prevFactor then
                    Some (getFactorValues head)
                else
                    getMinimalFactor tail
            | [] -> None
        Permutations.toList
        >> getMinimalFactor 

    let private tryGetMinFactor factors prevFactor permutationSize  =
        let nrOfItems = PermutationSize.value permutationSize
        let factorList = List.getListOfListWithMaxLength nrOfItems factors
        let nrOfFactors = factorList.Length
        let permutations = Permutations.create permutationSize (PermutationListSize nrOfFactors) 
        tryGetMin prevFactor (factorList |> List.toArray) permutations
        |> Option.bind (fun ls -> Some (Factor ls))

    let private remove factors  =
        let removeValueFromFactors value factors =
            match factors with
            | head :: tail ->
                if  head =  value then
                    true,tail
                else
                    false, factors
            | [] -> true, []
        let rec removeFactorFromFactors factorValue factors =
            match factors with
            | head :: tail ->
                let removed, rest = removeValueFromFactors factorValue head
                if removed then rest :: tail 
                else head :: removeFactorFromFactors factorValue tail
            | [] -> []

        toIntList
        >> List.fold (fun state toRemove -> removeFactorFromFactors toRemove state) factors
        >> List.filter (fun list -> list.Length > 0)

    let create prevFactor factors =
        let permutationSize = PermutationSize.calculate prevFactor factors
        let rec calculateFactors factorCount =
            let minFactor = tryGetMinFactor factors prevFactor (PermutationSize.create factorCount)
            let state = factorCount - 1
            if state <  0 then
                printfn "asdadsasda"
                [ ]
            else 
                match minFactor with
                | None -> calculateFactors (factorCount - 1)
                | Some factor -> factor ::  calculateFactors (factorCount - 1)
             
                    
        let result = calculateFactors (PermutationSize.value permutationSize)
        if result.Length = 0 then
            let factorList = factors |> List.concat |> Seq.toList
            Rest factorList
        else
            result |> List.minBy (fun factor -> toInt factor)
           
    //let private verifyLastFactor  =
    //    let veriftLastFactor' factors =
    //        let checkLastFactor lastFactor restfactors =
    //            match restfactors with
    //            | head :: tail -> 
    //                if (toInt head) >= (toInt lastFactor) then merge head lastFactor :: tail
    //                else lastFactor :: restfactors        
    //            | [] ->  [ lastFactor ]
                        
    //        match factors with
    //        | head :: tail -> checkLastFactor head tail
    //        | [] -> []
    //    List.rev
    //    >> veriftLastFactor'
    //    >> List.rev
    //let private verifyLastFactor  =
    //    let tryMergeFirst factors =
    //        match factors with
    //        | head :: tail -> 
    //            match head with
    //            | Rest _ -> 
    //                merge head tail.Head :: tail.Tail
    //            | Factor _ -> factors
    //        | [] -> factors
    //    List.rev 
    //    >>  tryMergeFirst
    //    >> List.rev
     
    let calculateOptimalFactors  =
        let rec getAllFactors' prevFactor factors =
            let factor = create prevFactor factors
            match factor with
            | Factor _ ->
                let state = remove factors factor
                factor :: getAllFactors' (toInt factor)  state
            | Rest -> []
        getAllFactors' 1 
     
let allFactors = Factor.calculateOptimalFactors factors



//verifyLastFactor [ 2 ; 3 ; 6; 7; 3 ]

        
            
        
        

//let factors1 = getAllFactors factors
//factors1.Length

   

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
