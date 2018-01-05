type nat = ZERO
    | SUCC of nat 

let rec natadd : nat * nat -> nat = fun (n1, n2) ->
    match n2 with
    | ZERO -> n1
    | SUCC a -> natadd ((SUCC n1), a)

let natmul : nat * nat -> nat = fun (n1, n2) ->
    let rec mul_sub : nat * nat * nat -> nat = fun (n1, n2, result) ->
        match n2 with
        | ZERO -> result
        | SUCC a -> mul_sub (n1, a, (natadd (result, n1)))
    in  
    mul_sub (n1, n2, ZERO)

(* my function *)
let rec string_of_nat : nat -> string = fun n ->
    match n with
    | ZERO -> "ZERO"
    | SUCC a -> "(SUCC " ^ (string_of_nat a) ^ ")" 

