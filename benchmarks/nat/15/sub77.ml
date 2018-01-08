type nat = ZERO | SUCC of nat

let rec natadd : (nat * nat) -> nat = fun(a,b) ->
   match b with
   | ZERO -> a
   | SUCC c -> 
      (match a with 
      |ZERO -> b
      |SUCC d -> SUCC(natadd(d,b)) 
      )

let rec natmul : (nat * nat) -> nat = fun(a,b) ->
   
   match b with
   | ZERO -> ZERO
   | SUCC ZERO -> a
   | SUCC (SUCC ZERO) -> natadd(a,a)
   | SUCC c ->
      (match a with
      |ZERO -> ZERO
      |SUCC ZERO -> b
      |SUCC (SUCC ZERO) -> natadd(b,b)
      |SUCC d -> 
          natadd(b, natmul(d, b))
      )
(*
let _ = 
let rec nat_to_int : nat -> int = 
fun n -> 
match n with 
| ZERO -> 0 
| SUCC n1 -> 1 + nat_to_int n1 
in 

let print_bool x = 
print_endline (string_of_bool x) 
in 

let three = SUCC (SUCC (SUCC ZERO)) 
in 
let four = SUCC three 
in 

print_bool (7 = nat_to_int (natadd (three, four))); 
print_bool (0 = nat_to_int (natadd (ZERO, ZERO))); 
print_bool (3 = nat_to_int (natadd (ZERO, three))); 
print_bool (4 = nat_to_int (natadd (four, ZERO))); 

print_bool (12 = nat_to_int (natmul (three, four))); 
print_bool (0 = nat_to_int (natmul (ZERO, three))); 
print_bool (0 = nat_to_int (natmul (four, ZERO))); 
print_bool (0 = nat_to_int (natmul (ZERO, ZERO))); 
print_bool (3 = nat_to_int (natmul (SUCC ZERO, three))); 
print_bool (4 = nat_to_int (natmul (four, SUCC ZERO))); 
*)