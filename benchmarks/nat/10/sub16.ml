type nat = ZERO | SUCC of nat
exception ERROR of string
let rec natadd nat_var1 nat_var2 =
  match nat_var2 with
    ZERO -> nat_var1
    |SUCC nat -> natadd (SUCC nat_var1) nat

let natmul nat_var1 nat_var2 =
  let rec iter result nat_var1 nat_var2 =
    match nat_var2 with
      SUCC ZERO -> result
      |SUCC nat_var -> iter (natadd result nat_var1) nat_var1 nat_var
      |_ -> raise (ERROR "Don't enter this code")
  in
  if (nat_var1 != ZERO) && (nat_var2 != ZERO) then iter nat_var1 nat_var1 nat_var2
  else ZERO

  
