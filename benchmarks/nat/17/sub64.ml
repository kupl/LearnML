type nat = ZERO | SUCC of nat

let rec natadd ((n1:nat),(n2:nat)):nat =
match (n1,n2) with
|(ZERO,_) -> n2
|(_,ZERO) -> n1
|(SUCC ZERO,_) -> SUCC n2
|(_,SUCC ZERO) -> SUCC n1
|(SUCC suc1,_) -> natadd (suc1,SUCC n2) 

let rec natmul ((n1:nat),(n2:nat)):nat = 
match (n1,n2) with 
|(ZERO,_) -> ZERO
|(_,ZERO) -> ZERO
|(SUCC ZERO,_) -> n2
|(_,SUCC ZERO) -> n1
|(SUCC suc1,_) -> natadd(n2,(natmul(suc1,n2)))


(*test
let _ =
let rec nat_to_int:nat->int = 
fun n ->
match n with
|ZERO->0
|SUCC n1 -> 1+nat_to_int n1
in

let print_bool x = 
print_endline (string_of_bool x)
in

let three = SUCC(SUCC(SUCC ZERO))
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
