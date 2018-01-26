(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> 
    let rec sub_cal: exp -> int -> int -> int
    = fun e n sum ->
        match e with
        | X -> n
        | INT x -> x
        | ADD (e1, e2) -> (sub_cal e1 n sum) + (sub_cal e2 n sum)
        | SUB (e1, e2) -> (sub_cal e1 n sum) - (sub_cal e2 n sum)
        | MUL (e1, e2) -> (sub_cal e1 n sum) * (sub_cal e2 n sum)
        | DIV (e1, e2) -> (sub_cal e1 n sum) / (sub_cal e2 n sum)
        | SIGMA (e1, e2, e3) -> 
                if e1 <= e2 then sub_cal 
                 (SIGMA (INT (sub_cal (ADD (e1, INT 1)) n sum), e2, e3)) 
                 (n+1) (sum+(sub_cal e3 n sum))
                else sum 
    in match e with 
    | SIGMA (e1, e2, e3) -> (sub_cal e (sub_cal e1 1 0) 0)
    | _ -> (sub_cal e 1 0);;
