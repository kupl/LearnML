(*
  1. You can modify the given function specifications as recursive.
  2. Do not modify the function names or types.
  3. It is free to define any helper functions.
*)

exception NotImplemented

(*********************)
(*     Problem 1     *)
(*********************)
module Problem1 = struct
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

  let rec diff : aexp * string -> aexp
= fun (aex, str) -> match aex with
    | Const n -> Const 0
    | Var a -> if a = str then Const 1 else Var a
    | Power (a,b) -> if a = str then Times[Const b; Power (a, b-1)] else Power (a,b)
    | Times a -> tImes (a, str)
    | Sum a -> sUm (a, str)
and tImes : aexp list * string -> aexp
= fun (lst, str) -> match lst with
    | [] -> Const 1
    | [h] -> diff (h, str)
    | hd::tl -> Sum([Times([diff (hd,str)]@tl)]@[Times ([hd]@[tImes (tl, str)])])
and sUm : aexp list * string -> aexp
= fun (lst, str) -> match lst with
    | [] -> Const 0
    | hd::tl -> Sum ([diff (hd,str)]@[sUm (tl, str)])
end

(*********************)
(*     Problem 2     *)
(*********************)
module Problem2 = struct
  type mobile = branch * branch
  and branch = 
  | SimpleBranch of length * weight
  | CompoundBranch of length * mobile
  and length = int
  and weight = int

let rec balanced : mobile -> bool
= fun mob -> match mob with
    | (a,b) -> if getTorque a 0 0 + getTorque b 0 1 = 0 then true else false
and getTorque : branch -> int -> int -> int
= fun br len dir -> match br with
    | SimpleBranch (a,b) -> if dir = 0 then (len + a)*b else (len - a)*b
    | CompoundBranch (length,(l,r)) -> if balanced (l,r) = true then (getTorque l (length+len) 0) + (getTorque r (len+length) 1) else -9999999999

end

(*********************)
(*     Problem 3     *)
(*********************)
module Problem3 = struct
  type exp =
  | X
  | INT of int
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | SIGMA of exp * exp * exp

  let rec calculator : exp -> int
= fun ex -> match ex with
    | INT n -> n
    | ADD (a,b) -> calculator a + calculator b
    | SUB (a,b) -> calculator a + calculator b
    | MUL (a,b) -> calculator a * calculator b
    | DIV (a,b) -> calculator a / calculator b
    | SIGMA (a,b,c) -> si (calculator a) (calculator b) c
and si : int -> int -> exp -> int
= fun st fi ex -> match ex with
    | SIGMA (a,b,c) -> si (calculator a) (calculator b) c
    | _ -> if st = fi then ass ex st else if st > fi then ass ex fi + si st (fi+1) ex else ass ex st + si (st+1) fi  ex
and ass : exp -> int -> int
= fun ex num -> match ex with
    | X -> num
    | INT n -> n
    | ADD (a,b) -> ass a num + ass b num
    | SUB (a,b) -> ass a num - ass b num
    | MUL (a,b) -> ass a num * ass b num
    | DIV (a,b) -> ass a num / ass b num
    | SIGMA (a,b,c) -> si (calculator a) (calculator b) c
end

(*********************)
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

  let rec check : exp -> bool
= fun ex -> match ex with
	| _ -> calc ex []
and calc : exp -> var list -> bool
= fun ex lst -> match ex with
	| V a -> deletion lst a
	| P (a,b) -> calc b (lst@[a])
	| C (a,b) -> if calc a lst = true && calc b lst = true then true else false
and deletion : var list -> var -> bool
= fun lst va -> match lst with
	| [] -> false
	| hd::tl -> if va = hd then true else (deletion tl va)
end

