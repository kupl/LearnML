type nat = ZERO 
           | SUCC of nat

let rec natadd (l, r) =
  match l with
  | ZERO -> r
  | SUCC l_ -> SUCC (natadd (l_, r))

let rec natmul (l, r) =
  match l with
  | ZERO -> ZERO
  | SUCC l_ -> natadd ((natmul (l_, r)), r)

(*
let rec eval n =
  match n with
  | ZERO -> 0
  | SUCC nn -> (eval nn) + 1

let _ =
  let print_bool x =
    print_endline (string_of_bool x) in 
    let three = SUCC (SUCC (SUCC ZERO)) in 
      let four = SUCC three in 
        print_bool (7 = eval (natadd (three, four))); 
        print_bool (0 = eval (natadd (ZERO, ZERO))); 
        print_bool (3 = eval (natadd (ZERO, three)));
        print_bool (4 = eval (natadd (four, ZERO))); 
        print_bool (12 = eval (natmul (three, four))); 
        print_bool (0 = eval (natmul (ZERO, three))); 
        print_bool (0 = eval (natmul (four, ZERO))); 
        print_bool (0 = eval (natmul (ZERO, ZERO))); 
        print_bool (3 = eval (natmul (SUCC ZERO, three))); 
        print_bool (4 = eval (natmul (four, SUCC ZERO))); 
        print_bool (2 = eval (natadd (ZERO, (SUCC (SUCC ZERO)))));
*)
