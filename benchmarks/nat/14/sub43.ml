

(* Author: Arif Jafer, 2012-11255 *)
(* PL, Spring 2014 *)

(* HW1-Q3: Natural Number *)




type nat = ZERO | SUCC of nat

let rec natadd = function
  | (ZERO, ZERO) -> ZERO
  | (ZERO, SUCC n) | (SUCC n, ZERO) -> SUCC n
  | (SUCC n, n2) -> SUCC (natadd (n, n2)) ;;



let rec natmul = function
  | (ZERO, n) | (n, ZERO) -> ZERO
  | (SUCC n, n2) -> natadd (n2, (natmul (n, n2))) ;;


(* Test Case

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


