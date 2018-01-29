(* problem 7*) 
type digit = ZERO | ONE 
type bin = digit list 

let bmul : bin -> bin -> bin = fun w1 w2 
-> let rec mlist : bin -> 'a list = fun b
-> match b with
| [] -> []
| head::tail -> if (head = ONE) then (mlist tail)@[1]
  else (mlist tail)@[0] in
let rec fastexpt : int -> int -> int = fun b n
-> if n = 0 then 1
 else if (n mod 2) = 0 then fastexpt b (n/2) * fastexpt b (n/2)
 else b * fastexpt b (n-1) in
let rec mdecimal : int -> 'a list -> int = fun n l
-> match l with
| [] -> 0
| head::tail -> (head * (fastexpt 2 n)) + (mdecimal (n+1) tail) in
let rec mbinary : int -> bin -> bin = fun n b
-> if n < 1 then b
else if (n mod 2) = 0 then mbinary (n/2) ([ZERO]@b)
else mbinary ((n-1)/2) ([ONE]@b) in
let v1 = mdecimal 0 (mlist(w1)) in
let v2 = mdecimal 0 (mlist(w2)) in
let v3 = v1 * v2 in
mbinary v3 []
