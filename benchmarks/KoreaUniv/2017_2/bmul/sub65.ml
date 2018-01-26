(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let rec bintoint b n =
    match b with
    | [] -> n
    | h::t ->
      if h = ZERO then (bintoint t (2*n))
      else if h = ONE then (bintoint t ((2*n) + 1))
      else raise (Failure "Invalid binary value") in
  let rec inttobin n b =
    if n = 0 then b
    else if n mod 2 = 0 then inttobin (n/2) (ZERO::b)
    else inttobin (n/2) (ONE::b) in
  if (b1 = []) || (b2 = []) then raise (Failure "Invalid Input")
  else inttobin ((bintoint b1 0)*(bintoint b2 0)) []