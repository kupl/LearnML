
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let calculator : exp -> int
  = fun exp -> (* TODO *)
  
  let rec map f l =
    (
    match l with
    |[] -> []
    |hd::tl -> (f hd)::(map f tl)
    )
  in
  
  let rec fold f l a =
    (
    match l with
    |[] -> a
    |hd::tl -> f hd (fold f tl a)
    )
  in
  
  let sumlist list = fold (fun x y -> x+y) list 0
  in
  
  let rec makelist startp endp =
    if (startp=endp) then [startp]
    else startp::( makelist (startp+1) endp )  
  in     
  
  let rec xcalculator = fun exp element ->
    (
    match exp with
    |X ->
      (
      match element with
      |[] -> raise (Failure "Must Use Value!")
      |hd::tl -> hd
      )
    |INT n -> n  
    |ADD (exp1, exp2) -> (xcalculator exp1 element) + (xcalculator exp2 element)
    |SUB (exp1, exp2) -> (xcalculator exp1 element) - (xcalculator exp2 element)
    |MUL (exp1, exp2) -> (xcalculator exp1 element) * (xcalculator exp2 element)
    |DIV (exp1, exp2) -> (xcalculator exp1 element) / (xcalculator exp2 element)  
    |SIGMA (exp1, exp2, exp3) ->
      let spoint = (xcalculator exp1 element) in
      let epoint = (xcalculator exp2 element) in
      let mylist = makelist spoint epoint in
      
      sumlist ( (map (fun element -> (xcalculator exp3 [element]) ) mylist) )
    ) 
  in  
  
  xcalculator exp []