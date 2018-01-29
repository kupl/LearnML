(* problem 3*)
type formula =
  True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula
  
  let sat : formula -> bool = fun f
  ->
  let rec getVar : formula -> (formula * bool)list -> (formula * bool)list -> (formula * bool)list = fun var l1 l2
  ->match l1 with
  [] -> [(var, true)]@l2
  | (a, b)::tail -> if (a = var) then l2
          else getVar var tail l2 in
  
  let rec getTable : formula -> (formula * bool) list -> bool = fun var l
  -> match l with
  [] -> true
  | (a, b)::tail -> if var = a then b
          else getTable var tail in
  
  
  let rec makeList : formula -> (formula * bool) list -> (formula * bool) list = fun f l
  -> match f with
  True -> []
  | False -> []
  | Var a -> getVar (Var a) [] []
  | Neg a -> makeList a l
  | And (a, b) -> (makeList a l)@(makeList b l)
  | Or (a, b) -> (makeList a l)@(makeList b l)
  | Imply (a, b) -> (makeList a l)@(makeList b l)
  | Iff (a, b) -> (makeList a l)@(makeList b l) in
  
  let makedTable = makeList f [] in
  
  let changeBool : (formula * bool) list -> (formula * bool) list = fun l
  -> match l with
  [] -> []
  | (a, b)::tail -> if b = true then ((a, false)::tail) else tail in
  
  let rec formulatbl : formula -> (formula * bool) list -> bool = fun f table
  -> match f with
  True -> true
  | False -> false
  | Var a -> getTable (Var a) table
  | Neg a -> not(formulatbl a table)
  | And (a, b) -> (formulatbl a table) && (formulatbl b table)
  | Or (a, b) -> (formulatbl a table) || (formulatbl b table)
  | Imply (a, b) -> if ((formulatbl a table) = true) && ((formulatbl b table) = false) then false else true
  | Iff (a, b) -> if (formulatbl a table) = (formulatbl b table) then true else false in
  
  let rec eval : (formula * bool) list -> bool = fun l
  -> match l with
  [] -> false
  | (a,b)::tail -> b || (eval tail) in
  
  let rec evaltbl : formula -> (formula * bool) list -> bool = fun f l
  -> if (eval l) = false then formulatbl f l
  else (formulatbl f l) || (evaltbl f (changeBool l)) in
  evaltbl f makedTable;;