type exp = 
  X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun exp -> match exp with
  |X -> 0
  |INT n -> n
  |ADD(exp1, exp2) -> (calculator exp1) + (calculator exp2)
  |SUB(exp1, exp2) -> (calculator exp1) - (calculator exp2)
  |MUL(exp1, exp2) -> (calculator exp1) * (calculator exp2)
  |DIV(exp1, exp2) -> (calculator exp1) / (calculator exp2)
  |SIGMA(exp1, exp2, exp3) ->
    let rec make_func ex=
      match ex with
        |X -> (fun x -> x)
        |INT n -> (fun x -> n)
        |ADD(exp1, exp2) -> (fun x -> ((make_func exp1) x) + ((make_func exp2) x))
        |SUB(exp1, exp2) -> (fun x -> ((make_func exp1) x) - ((make_func exp2) x))
        |MUL(exp1, exp2) -> (fun x -> ((make_func exp1) x) * ((make_func exp2) x))
        |DIV(exp1, exp2) -> (fun x -> ((make_func exp1) x) / ((make_func exp2) x))
        |_ -> (fun x -> 0)
    in let rec compute_sigma n1 n2 f=
      if n1 = n2 then f n2
      else (f n1) + (compute_sigma (n1+1) n2 f)
    in compute_sigma (calculator exp1) (calculator exp2) (make_func exp3);;
    

  