(* problem 5 *)
type exp = X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

exception Foo of string

let rec calculator : exp -> int = fun e ->
  let rec exptofunc : exp -> (int -> int) = fun f -> 
    (match f with
     | X -> (fun x -> x)
     | INT n -> (fun x -> n)
     | ADD (e1, e2) ->
        (fun x -> (exptofunc e1 x) + (exptofunc e2 x))
     | SUB (e1, e2) ->
        (fun x -> (exptofunc e1 x) - (exptofunc e2 x))
     | MUL (e1, e2) ->
        (fun x -> (exptofunc e1 x) * (exptofunc e2 x))
     | DIV (e1, e2) ->
        (fun x -> (exptofunc e1 x) / (exptofunc e2 x))
     | _ -> raise (Foo "Too Much Sigma")
    )
  in
  let rec calsigma : int -> int -> (int -> int) -> int = fun n0 n f ->
    if n0 > n then 0 else
      (f n0) + (calsigma (n0 + 1) n f)
  in
  match e with
  | INT n -> n
  | ADD (e1, e2) -> (calculator e1) + (calculator e2)
  | SUB (e1, e2) -> (calculator e1) - (calculator e2)
  | MUL (e1, e2) -> (calculator e1) * (calculator e2)
  | DIV (e1, e2) -> (calculator e1) / (calculator e2)
  | SIGMA (n1, n2, f) -> calsigma (calculator n1) (calculator n2) (exptofunc f)
  | _ -> raise (Foo "No Info of X")