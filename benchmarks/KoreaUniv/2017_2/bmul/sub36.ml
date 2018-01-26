type digit = ZERO | ONE
type bin = digit list

let rec bmul : bin -> bin -> bin
= fun bin1 bin2 ->
  let rec last l = 
    match l with
    | [x] -> x
    | x::xs -> last xs
    | [] -> raise (Failure "empty list input on last<func>")
  in
  let rec init l = 
    match l with
    | [x] -> []
    | x::xs -> [x]@(init xs)
    | [] -> raise (Failure "empty list input on init<func>")
  in
  let rec length l = 
    match l with
    | _::xs -> 1 + (length xs)
    | _ -> 0
  in
  let addDigits x = 
    match x with
    | (ZERO, ZERO, ZERO) -> (ZERO, ZERO)
    | (ZERO, ZERO, ONE) -> (ONE, ZERO)
    | (ZERO, ONE, ZERO) -> (ONE, ZERO)
    | (ONE, ZERO, ZERO) -> (ONE, ZERO)
    | (ZERO, ONE, ONE) -> (ZERO, ONE)
    | (ONE, ZERO, ONE) -> (ZERO, ONE)
    | (ONE, ONE, ZERO) -> (ZERO, ONE)
    | (ONE, ONE, ONE) -> (ONE, ONE)
  in
  let lengthCheck l = if l=[] then false else true
  in
  let rec zeros n = if n=0 then [] else ZERO::(zeros (n-1))
  in
  let rec bsum bin1 bin2 x = 
    if (lengthCheck bin1) && (lengthCheck bin2) 
    then let k = addDigits (last bin1, last bin2, x)
         in (bsum (init bin1) (init bin2) (snd k))@[fst k]
    else if x=ONE
         then bsum (bin1@bin2) [ONE] ZERO
         else bin1@bin2
  in
  let rec bmul_ bin1 bin2 n bin3 = 
    match bin2 with
    | ONE::xs -> bmul_ bin1 xs (n-1) (bsum bin3 (bin1@(zeros (n-1))) ZERO)
    | ZERO::xs -> bmul_ bin1 xs (n-1) bin3
    | [] -> bin3
  in
  bmul_ bin1 bin2 (length bin2) [ZERO]
