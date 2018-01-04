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
(*    
let rec nat2int : nat -> int = function
  a ->
    match a with
      | ZERO -> 0
      | SUCC(n) -> 1 + nat2int(n)
        
let zero = ZERO
and one = SUCC(ZERO)
and two = SUCC(SUCC(ZERO))
let three = SUCC(two)
let four = SUCC(three)

let _ = 
let print_int x = 
print_endline (string_of_int x) in 
  print_int (nat2int (natadd(four, three))); 
  print_int (nat2int (natmul(four, three))); 
 *)
          