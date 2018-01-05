(* 2014 - 18474 kim ju hyeon  *)
(* 2017 fall PL 1_4 *)
type nat = ZERO | SUCC of nat
let rec natadd : nat * nat -> nat = function
  (a, b) ->
    match a with
      | ZERO -> b
      | SUCC(a_minus_1) -> natadd(a_minus_1, SUCC(b))
            
let rec natmul : nat * nat -> nat = function
  (a, b) ->
    match a with
      | ZERO -> ZERO
      | SUCC(a_minus_1) -> natadd(b, natmul(a_minus_1, b))