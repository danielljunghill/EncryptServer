open System
open System.Collections
let bitArr = new BitArray(Int32.MaxValue,true)

//let getPrimes maxValue =
//    let rec getPrimes' (bitArr: BitArray) index prime =
//        seq {
//        if index * prime > bitArr.Length then
//            //get next free
//            let rec tryGetNextPrime (bitArr: BitArray) prime =
//                let next = prime + 1
//                if next > bitArr.Length then None
//                else
//                    if bitArr.[next - 1] then Some next
//                    else tryGetNextPrime bitArr next
//            match tryGetNextPrime bitArr prime with
//            | None ->   yield prime
//            | Some p ->  
//                yield prime
//                yield! getPrimes' bitArr 1 p
//        else 
//            bitArr.[index * prime - 1] <- false
//            yield! getPrimes' bitArr (index + 1) prime
//            }
//    getPrimes' (new BitArray(maxValue,true)) 1 2

//let getPrimes maxValue =
//    let rec getPrimes' state (bitArr: BitArray) index prime =
//        seq {
//        if index * prime > bitArr.Length then
//            //get next free
//            let rec tryGetNextPrime (bitArr: BitArray) prime =
//                let next = prime + 1
//                if next > bitArr.Length then None
//                else
//                    if bitArr.[next - 1] then Some next
//                    else tryGetNextPrime bitArr next
//            match tryGetNextPrime bitArr prime with
//            | None ->   yield prime
//            | Some p ->  
//                let newState = prime :: state
//                yield prime
//                yield! getPrimes' newState bitArr 1 p
//        else 
//            bitArr.[index * prime - 1] <- false
//            yield! getPrimes' state bitArr (index + 1) prime
//            }
//    getPrimes' List.empty (new BitArray(maxValue,true)) 1 2


let getPrimes maxValue =
    let rec getPrimes' state (bitArr: BitArray) index prime =
        if index * prime > bitArr.Length then
            //get next free
            let rec tryGetNextPrime (bitArr: BitArray) prime =
                let next = prime + 1
                if next > bitArr.Length then None
                else
                    if bitArr.[next - 1] then Some next
                    else tryGetNextPrime bitArr next
            match tryGetNextPrime bitArr prime with
            | None ->   
                let newState = state @ [prime]
                newState
            | Some p ->  
                let newState = state @ [prime]
                getPrimes' newState bitArr 1 p
        else 
            bitArr.[index * prime - 1] <- false
            getPrimes' state bitArr (index + 1) prime
            
    getPrimes' List.empty (new BitArray(maxValue,true)) 1 2




let t = getPrimes 123112

//let r = 11 % 5

let rec factorize' state prime =
    let (number, factors) = state
    match number % prime with
    | 0 ->  

        factorize' (number / prime ,prime :: factors) prime
    | _ ->  state

factorize' (15,[]) 3


let factorize number =
    let rec getPrimes' state (bitArr: BitArray) index prime =
        if index * prime > bitArr.Length then
            //get next free
            let rec tryGetNextPrime (bitArr: BitArray) prime =
                let next = prime + 1
                if next > bitArr.Length then None
                else
                    if bitArr.[next - 1] then Some next
                    else tryGetNextPrime bitArr next
            match tryGetNextPrime bitArr prime with
            | None ->   state
            | Some p ->  
                let number,factors = factorize' state prime
                if number = 1 then
                    (number,factors)
                else
                    getPrimes' (number,factors) bitArr 1 p

        else 
            bitArr.[index * prime - 1] <- false
            getPrimes' state bitArr (index + 1) prime
            
    getPrimes' (number,List.empty) (new BitArray(number,true)) 1 2
factorize (15*13*12*7*54)
module List =
    let int = List.fold (fun s i -> s* i) 1

module Prime =
    let factorize number =
        getPrimes number
        |> Seq.fold ([] primeNr -> factorize )

   
let t = number %           

       