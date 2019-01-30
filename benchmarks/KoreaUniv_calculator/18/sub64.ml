type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp


let rec modifyX : exp -> int -> exp
= fun ex n ->
  match ex with
  | X -> INT n
  | INT a -> INT a
  | ADD (ex1, ex2) -> ADD (modifyX ex1 n, modifyX ex2 n)
  | SUB (ex1, ex2) -> SUB (modifyX ex1 n, modifyX ex2 n)
  | MUL (ex1, ex2) -> MUL (modifyX ex1 n, modifyX ex2 n)
  | DIV (ex1, ex2) -> DIV (modifyX ex1 n, modifyX ex2 n)
  | SIGMA (ex1, ex2, ex3) -> SIGMA (modifyX ex1 n, modifyX ex2 n, ex3) ;;

 
let rec calculator : exp -> int
= fun exp ->
  match exp with
  | X -> 0
  | INT a -> a
  | ADD (ex1, ex2) -> calculator ex1 + calculator ex2
  | SUB (ex1, ex2) -> calculator ex1 - calculator ex2
  | MUL (ex1, ex2) -> calculator ex1 * calculator ex2
  | DIV (ex1, ex2) -> calculator ex1 / calculator ex2
  | SIGMA (ex1, ex2, ex3) ->
    let rec sigma : int -> int -> exp -> int -> int
    = fun cnt end_ ex result ->
      if cnt <= end_
        then sigma (cnt+1) end_ ex (result+calculator(modifyX ex cnt))
      else result
    in sigma (calculator ex1) (calculator ex2) ex3 0;;
    
    
