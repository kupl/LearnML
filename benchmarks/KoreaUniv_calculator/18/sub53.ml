type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> 
    let rec sigCal expression subX =
      match expression with
        X -> subX
        |INT i -> i
        |ADD (e1, e2) -> (sigCal e1 subX) + (sigCal e2 subX)
        |SUB (e1, e2) -> (sigCal e1 subX) - (sigCal e2 subX)
        |MUL (e1, e2) -> (sigCal e1 subX) * (sigCal e2 subX)
        |DIV (e1, e2) -> (sigCal e1 subX) / (sigCal e2 subX)
        |SIGMA (f, l, e) -> if sigCal f subX > sigCal l subX then raise (Failure "ERROR: Starting value is bigger than last value.") else
          let rec range n1 n2 = if (n1 = n2) then [n1] else n1 :: (range (n1 + 1) n2) in
          let toCalculate = range (sigCal f subX) (sigCal l subX) in
          let rec fmap f lst = 
            match lst with
              [] -> [] | x::xs -> (f x)::(fmap f xs) in
          let rec foldr f acc lst = 
            match lst with
              [] -> acc | x::xs -> f x (foldr f acc xs) in
          foldr (fun x1 x2 -> x1 + x2) 0 (fmap (sigCal e) toCalculate)
    in sigCal exp 0;;
   