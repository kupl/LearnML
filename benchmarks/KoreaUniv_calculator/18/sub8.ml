type exp = 
    X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;

let rec cal : exp -> int -> int
= fun abv integer ->
  match abv with
       X -> integer
    | INT inte -> inte
    | ADD (inte1, inte2) -> (cal inte1 integer) + (cal inte2 integer)
    | SUB (inte1, inte2) -> (cal inte1 integer) - (cal inte2 integer)
    | MUL (inte1, inte2) -> (cal inte1 integer) * (cal inte2 integer)
    | DIV (inte1, inte2) -> (cal inte1 integer) / (cal inte2 integer)
    | SIGMA (inte1, inte2, inte3) ->
      let i = cal inte1 integer in
      let n = cal inte2 integer in
      if (i = n) then cal inte3 i
      else (cal inte3 i) + cal (SIGMA (INT(i+1), inte2, inte3)) i
      ;;

let rec calculator : exp -> int
= fun exp -> 
  cal exp 0 ;;
  
  