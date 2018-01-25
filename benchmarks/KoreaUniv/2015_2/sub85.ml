(*problem 1*)
let rec filter p l = match l with 
   [] -> []
   | (first :: left) -> 
       if (p first) then first :: (filter p left) else (filter p left)

(*problem 2*)
let rec zipper (a, b) = match (a, b) with
   | _, [] -> a
   | [], _ -> b
   | first1 :: left1, first2 :: left2 ->
       if first1 < first2 then first1 :: zipper (left1, b) else first2 :: zipper (a, left2)

(*problem 3*)
let rec iter (n, f) =
	if n<=0 then (function x->x) else (function x->iter(n-1, f) (f x))

(*problem 4*)

(*problem 5*)
exception FreeVariable

type exp = X
    | INT of int
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | SIGMA of exp * exp * exp

let rec calc l x = match l with
    | X ->
      (match x with
      | None -> raise FreeVariable
      | Some s -> s)
    | INT i -> i
    | ADD (a, b) -> (calc a x) + (calc b x)
    | SUB (a, b) -> (calc a x) - (calc b x)
    | MUL (a, b) -> (calc a x) * (calc b x)
    | DIV (a, b) -> (calc a x) / (calc b x)
    | SIGMA (a, b, c) -> 
      let y1 = calc a x in
      let y2 = calc b x in
      let rec sigma i res =
        if i > y2 then res else 
	  let v = calc c (Some i) in
          sigma (i + 1) (v + res) in
      	  sigma y1 0

let calculator l = calc l None;;






