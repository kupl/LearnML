
(* ------------------problem5------------------ *)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e ->
	let rec div f =
      (match f with
      X -> raise (Failure "Not Assigned")  |
      INT n -> n |
      ADD (e1, e2) -> (div e1) + (div e2) |
      SUB (e1, e2) -> (div e1) - (div e2) |
      MUL (e1, e2) -> (div e1) * (div e2) |
      DIV (e1, e2) -> (div e1) / (div e2) |
      SIGMA (e1, e2, e3) -> 

      let rec calulate k =
         (match k with
         X -> (fun x->x) |
         INT n -> (fun x->n) |
         ADD (e1,e2) -> (fun x->((calulate e1) x)+((calulate e2) x)) |
         SUB (e1,e2) -> (fun x->((calulate e1) x)-((calulate e2) x)) |
         MUL (e1,e2) -> (fun x->((calulate e1) x)*((calulate e2) x)) |
         DIV (e1,e2) -> (fun x->((calulate e1) x)/((calulate e2) x)) |
         SIGMA (e1,e2,e3) -> raise (Failure "test,,,"))
       in

       let rec sigma s f =
         if s>f then 0
         else if s=f then ((calulate e3) s)
         else (((calulate e3) s) + (sigma (s+1) f))
       in (sigma (div e1) (div e2)))
    in

    match e with
    X -> 0 |
    INT n -> n |
    ADD (e1,e2) -> (div (ADD (e1,e2))) |
    SUB (e1,e2) -> (div (SUB (e1,e2))) |
    MUL (e1,e2) -> (div (MUL (e1,e2))) |
    DIV (e1,e2) -> (div (DIV (e1,e2))) |
    SIGMA (e1,e2,e3) -> (div (SIGMA (e1,e2,e3)));;