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
  = fun (exp, var) ->
  match exp with
  | Const n -> Const 0
  | Var x -> if x = var then Const 1 else Const 0
  | Power (x, n) -> if x = var then Times[Const n; Power(x, n - 1)] else Const 0
  | Times [] -> Const 0
  | Times (hd::[]) -> diff(hd,var)
  | Times (hd::tl) -> Sum[Times(diff(hd, var):: tl); Times[hd; diff(Times tl, var)]]
  | Sum [] -> Const 0
  | Sum (hd::[]) -> diff(hd, var) 
  | Sum (hd::tl) -> Sum[diff(hd, var); diff(Sum tl, var)]
end





(***********************************)
(**            Problem 2          **)
(***********************************)


module Problem2 = struct
type mobile = branch * branch
  and branch = SimpleBranch of length * weight
             | CompoundBranch of length * mobile
  and length = int
  and weight = int
  


let rec sumOfWeight
=fun br ->
   match br with
      | SimpleBranch(l,w)->w
      | CompoundBranch(l,(lb,rb))->(sumOfWeight lb)+(sumOfWeight rb);;


let multiple 
=fun br -> match br with
    | SimpleBranch (l, w) -> l * w
    | CompoundBranch (l, (lb,rb)) -> l * ((sumOfWeight lb) + (sumOfWeight rb));;

let rec balanced : mobile -> bool
=fun (lb,rb) ->match lb with
                            | SimpleBranch (l1, w1) -> (match rb with
                                                      | SimpleBranch (l2, w2) -> if ((multiple lb) = (multiple rb)) then true
                                                                                  else false
                                                      | CompoundBranch (l2,(lb1,rb1)) -> if(( balanced (lb1, rb1) = true) && ((multiple lb) = (multiple rb))) then true
                                                                                        else false)
                            | CompoundBranch (l1, (lb1, rb1)) ->  if( balanced (lb1, rb1) = false) then false
                                                                  else (match rb with
                                                                      | SimpleBranch (l2,w2) -> if ((multiple lb) = (multiple rb)) then true
                                                                                                else false
                                                                      | CompoundBranch (l2, (lb2, rb2)) -> if (balanced (lb2, rb2) = false) then false
                                                                                                           else if ((multiple lb) = (multiple rb)) then true
                                                                                                         else false);;
                                                                  
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
  = fun exp ->
  match exp with
  | X -> raise(Failure "Wrong input")
  | INT m -> m
  | ADD (m, n) -> calculator m + calculator n
  | SUB (m, n) -> calculator m - calculator n
  | MUL (m, n) -> calculator m * calculator n
  | DIV (m, n) -> calculator m / calculator n
  | SIGMA(start, last, polynomial) ->
  let s = calculator start in
    let l = calculator last in
    if s = l then
    begin
    let rec helper = fun p la ->
    match p with
    | X -> helper la la
    | INT n -> n
    | ADD (m, n) -> (helper m la) + (helper n la)
    | SUB (m, n) -> (helper m la) - (helper n la)
    | MUL (m, n) -> (helper m la) * (helper n la)
    | DIV (m, n) -> (helper m la) / (helper n la)
    | SIGMA (st, las, pol) -> calculator p
    in helper polynomial last
    end
   else
  calculator (SIGMA (start, SUB(last, INT 1), polynomial)) + calculator (SIGMA(last, last, polynomial))
end



(*********************)
(*     Problem 4     *)
(*********************)


module Problem4 = struct 
  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  

  let check : exp -> bool
  =fun e -> true;;


  let rec isFreeVar : var * string list -> bool 
  =fun (v, l) -> match l with
                            | [] -> false
                            | hd :: tl -> if (v = hd) then true else isFreeVar (v,tl);;

  let rec findVar : exp * string list -> bool
  = fun (e, l) -> match e with
                  | V ((a:var)) -> isFreeVar (a, l)
                  | P ((v:var),e1) -> findVar (e1, (l @ [v]))
                  | C (e1,e2) -> if (findVar(e1,l) && findVar(e2,l))= true then true else false;;


  let check  : exp -> bool
  =fun e -> findVar (e, []);;

end
