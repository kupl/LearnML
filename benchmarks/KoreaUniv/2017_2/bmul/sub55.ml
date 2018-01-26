(*Problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec rev dl = 
  match dl with
  |[] -> []
  |hd::tl -> (rev tl) @ [hd]

let rec todec dl =
  match rev dl with
  |[] -> raise (Failure "error")
  |[ZERO] -> 0
  |[ONE] -> 1
  |hd::tl -> (todec [hd]) + 2*(todec (rev tl))

let rec tobin n = 
  match n with
  |0 -> [ZERO]
  |1 -> [ONE]
  |_ -> (tobin (n/2))@(tobin (n mod 2))  

let rec bmul dl1 dl2 = tobin ((todec dl1) * (todec dl2))































  
