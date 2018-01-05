type nat = ZERO | SUCC of nat;;

let rec natadd (a, b) = 
    match a with
    |ZERO -> b
    |SUCC aprev -> natadd (aprev, SUCC b);;

let rec natmul (a, b) =
    match a with
    |ZERO -> ZERO
    |SUCC aprev -> natadd (b, natmul (aprev, b));;
