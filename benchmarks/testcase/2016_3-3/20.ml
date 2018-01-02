type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec sigma : int * int * (int -> int) -> int
= fun (n1, n2, f) -> 
  if (n1 = n2) then f n1 else (f n1) + (sigma (n1 + 1, n2, f));;

let rec xcal : exp -> (int -> int)
= fun exp ->
   match exp with
   | X -> (fun x-> x)
   | INT n -> (fun x-> n)
   | ADD (e1, e2) -> (fun x -> (((xcal e1) x) + ((xcal e2) x)))
   | SUB (e1, e2) -> (fun x -> (((xcal e1) x) - ((xcal e2) x)))
   | MUL (e1, e2) -> (fun x -> (((xcal e1) x) * ((xcal e2) x)))
   | DIV (e1, e2) -> (fun x -> (((xcal e1) x) / ((xcal e2) x)))
   
let rec cal : exp -> int
= fun exp -> 
   match exp with
   | INT n -> n
   | ADD (e1, e2) -> (cal e1) + (cal e2)
   | SUB (e1, e2) -> (cal e1) - (cal e2)
   | MUL (e1, e2) -> (cal e1) * (cal e2)
   | DIV (e1, e2) -> (cal e1) / (cal e2)
   | SIGMA (e1, e2, e3) -> sigma (cal e1, cal e2, (xcal e3));;

let calculator : exp -> int
= fun exp -> 
   match exp with
   | X -> 0
   | INT n -> n
   | ADD (e1, e2) -> (cal (ADD (e1, e2)))
   | SUB (e1, e2) -> (cal (SUB (e1, e2)))
   | MUL (e1, e2) -> (cal (MUL (e1, e2)))
   | DIV (e1, e2) -> (cal (DIV (e1, e2)))
   | SIGMA (e1, e2, e3) -> (cal (SIGMA (e1, e2, e3)));;