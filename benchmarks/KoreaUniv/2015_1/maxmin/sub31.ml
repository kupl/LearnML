let rec max : int list -> int
=fun l -> 1 
match l with 
  |[]->0
  |h::t-> if h>(max t) then h 
  else (max t)

let rec min : int list -> int
=fun l -> 1
 match l with 
  |[]->0
  |h::t-> if h<(min t) then h 
  else (min t)
