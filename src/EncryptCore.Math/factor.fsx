#load "Factorization.fs"
open EncryptCore.Math
let factors =
    [ 
        [ 2 ; 2 ; 2 ; 2 ; 2 ; 2 ; 2]
        [ 3 ; 3 ; 3 ]
        [ 11 ; 11 ]
        [ 23 ]
        [ 31 ; 31 ; 31 ; 31 ; 31 ; 31]
        [ 53 ; 53 ; 53; 53; 53]
        [ 67 ]
      
    ]


let factorList = Factor.calculateOptimalFactors factors
let factors1 = Prime.factorize (13*12*7*57*14)
factors1 |> List.groupBy (fun v -> v) |> List.sort |> List.map (fun (_,v) -> v)
|> Factor.calculateOptimalFactors

