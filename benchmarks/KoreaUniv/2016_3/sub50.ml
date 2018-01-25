(*  Problem1  *)
module Problem1 = struct
type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff (f,var) = match f with
| Const x -> Const 0
| Var x -> if x=var then Const 1 else Const 0
| Power (x,y) -> if var=x then Times [Const y; Power (x,y-1)] else Const 0
| Times l -> (match l with
          | [] -> Const 0 
          | hd::tl -> Sum [Times (diff (hd,var)::tl); Times [hd;diff (Times tl, var)]])
| Sum l -> match l with
          | [] -> Const 0 
          | hd::tl -> Sum [diff (hd,var); diff (Sum tl,var)];; 
end

(*  Problem2  *)
module Problem2 = struct
type mobile = branch * branch
and branch = SimpleBranch of length * weight
            | CompoundBranch of length * mobile
and length = int
and weight = int

let rec returnweight c = match c with
| (SimpleBranch (x,y), SimpleBranch (a,b)) -> if (x*y)=(a*b) then y+b else -1
| (SimpleBranch (x,y), CompoundBranch (a,b)) -> if (x*y)=(a*(returnweight b)) then y+(returnweight b) else -1
| (CompoundBranch (x,y), SimpleBranch (a,b)) -> if (x*(returnweight y))=(a*b) then (returnweight y)+b else -1
| (CompoundBranch (x,y), CompoundBranch (a,b)) -> if (x*(returnweight y))=(a*(returnweight b)) then ((returnweight y)+(returnweight b)) else -1

let rec balanced : mobile -> bool = fun f -> match f with
| (SimpleBranch (x,y), SimpleBranch (a,b)) -> if (x*y)=(a*b) then true else false
| (SimpleBranch (x,y), CompoundBranch (a,b)) -> if (x*y)=(a*(returnweight b)) then true else false
| (CompoundBranch (x,y), SimpleBranch (a,b)) -> if (x*(returnweight y))=(a*b) then true else false
| (CompoundBranch (x,y), CompoundBranch (a,b)) -> if (x*(returnweight y))=(a*(returnweight b)) then true else false;;
end
 
(*  Problem3  *)
module Problem3 = struct
type exp = 
      | X
      | INT of int
      | ADD of exp * exp
      | SUB of exp * exp
      | MUL of exp * exp
      | DIV of exp * exp
      | SIGMA of exp * exp * exp

let rec calculator f = match f with
| X -> raise (Failure"X must be int type.")
| INT x -> x
| ADD (x,y) -> let v1 = calculator x in let v2 = calculator y in v1+v2
| SUB (x,y) -> let v1 = calculator x in let v2 = calculator y in v1-v2
| MUL (x,y) -> let v1 = calculator x in let v2 = calculator y in v1*v2
| DIV (x,y) -> let v1 = calculator x in let v2 = calculator y in v1/v2
| SIGMA (x,y,z) -> let v1 = calculator x in let v2 = calculator y in
if v1=v2 then sigma z v1 else (sigma z v1)+(calculator (SIGMA (ADD (x, INT 1),y,z)))
and sigma fu x1 = match fu with
| X -> x1
| INT q -> q
| ADD (q,w) -> let b1 = sigma q x1 in let b2 = sigma w x1 in b1+b2
| SUB (q,w) -> let b1 = sigma q x1 in let b2 = sigma w x1 in b1-b2
| MUL (q,w) -> let b1 = sigma q x1 in let b2 = sigma w x1 in b1*b2
| DIV (q,w) -> let b1 = sigma q x1 in let b2 = sigma w x1 in b1/b2
| SIGMA (q,w,r) -> let b1 = sigma q x1 in let b2 = sigma w x1 in
if b1=b2 then sigma r b1 else (sigma r b1)+(calculator (SIGMA (ADD(q,INT 1),w,r)));;
end
 

(*  Problem4  *)
module Problem4 = struct
type exp = V of var
         | P of var * exp
         | C of exp * exp
and var = string

let empty_env = []
let extend_env x e = x::e
let rec lookup_env x e = match e with
| [] -> false
| hd::tl -> if x=hd then true else lookup_env x tl

let rec check f = let rec _check fu en =
  match fu with
  | P (x,y) -> let env' = extend_env x en in _check y env'
  | V x -> if lookup_env x en then true else false
  | C (x,y) -> if _check x en = true then _check y en else false
in _check f empty_env;;
end
