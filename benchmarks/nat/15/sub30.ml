(* hw1 ex5 "Natural Number" *)


type nat = ZERO
         | SUCC of nat



let rec natadd (a,b) : nat = 
  match a,b with
    | ZERO , ZERO -> ZERO
    | _ , ZERO -> a
    | ZERO , _ -> b
    | SUCC (suba), _ -> natadd (suba, SUCC b)


let rec natmul (a,b) : nat = 
  match a,b with
    | ZERO , _ -> ZERO
    | _ , ZERO -> ZERO
    | SUCC (ZERO) , _ -> b
    | _, SUCC (ZERO) -> a
    | SUCC (suba), _ -> natmul (suba, natadd (a, b))


