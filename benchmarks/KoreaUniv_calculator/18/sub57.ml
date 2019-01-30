type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp;;

let rec calculator : exp -> int
= fun exp -> (*TODO*)

  let rec cal_SIGMA min max exp result = 
    
    let rec assignX x exp = 
      match exp with
        | X -> x
        | INT a -> a
        | ADD (myexp1,myexp2) -> assignX x myexp1 + assignX x myexp2
        | SUB (myexp1,myexp2) -> assignX x myexp1 - assignX x myexp2
        | MUL (myexp1,myexp2) -> assignX x myexp1 * assignX x myexp2
        | DIV (myexp1,myexp2) -> assignX x myexp1 / assignX x myexp2
        | SIGMA (minexp,maxexp,myexpX) -> 
          let min = assignX x minexp and max = assignX x maxexp in
          cal_SIGMA min max myexpX 0
    in
    
    if min<=max then cal_SIGMA (min+1) max exp ( result+(assignX min exp) )
    else result
  in
  match exp with
    | X -> 0 (*Warning 8: this pattern-matching is not exhaustive. 없애려고*)
    | INT x -> x
    | ADD (myexp1,myexp2) -> calculator myexp1 + calculator myexp2
    | SUB (myexp1,myexp2) -> calculator myexp1 - calculator myexp2
    | MUL (myexp1,myexp2) -> calculator myexp1 * calculator myexp2
    | DIV (myexp1,myexp2) -> calculator myexp1 / calculator myexp2
    | SIGMA (minexp,maxexp,myexpX) ->
      let min = calculator minexp and max = calculator maxexp in
      cal_SIGMA min max myexpX 0;;
