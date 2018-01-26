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
  open List
  type aexp = 
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list




  let rec diff : aexp * string -> aexp
  = fun (exp, var) ->  (* TODO *)
  match exp with
  |Const n -> Const 0
  |Var x -> if x = var then Const 1 else Const 0
  |Power (x,integer) -> if x = var then Times [Const integer;Power(x,integer-1)] else Const 0
  |Sum aexplist ->
    (match aexplist with
    |hd::tl -> if tl != [] then Sum ( diff (hd,var) :: [diff (Sum tl, var)] ) else diff (hd,var)
    )
  |Times aexplist -> 
    (match aexplist with
    |hd::tl -> if tl != [] then Sum [Times [diff (hd,var);Times tl];Times[hd;diff(Times tl,var)]] 
                else diff (hd,var)
    )
    

  
  let a = Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1]

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


  let rec branchtoweight : branch -> int
  = fun brc ->
  begin
      match brc with
      |SimpleBranch (_,w) -> w
      |CompoundBranch (_,(b1,b2)) -> branchtoweight b1+ branchtoweight b2
  end



  let rec balanced : mobile -> bool
  = fun mob ->  (* TODO *)
  begin
    match mob with
    |SimpleBranch (l1,w1) , SimpleBranch (l2,w2) -> if (l1*w1 != l2*w2) then false else true
    |CompoundBranch (l1,(b1,b2)) , SimpleBranch (l2,w) -> if (l1*(branchtoweight b1) + l1*(branchtoweight b2) != l2*w) then false else balanced (b1,b2)
    |SimpleBranch (l2,w) , CompoundBranch (l1,(b1,b2)) -> if (l1*(branchtoweight b1) + l1*(branchtoweight b2) != l2*w) then false else balanced (b1,b2)
    |CompoundBranch (l1,(b1,b2)) , CompoundBranch (l2,(b3,b4)) -> if (l1*(branchtoweight b1) + l1*(branchtoweight b2) != l2*(branchtoweight b3) + l2*(branchtoweight b4)) then false else balanced (b1,b2) && balanced (b3,b4)
  end 


let a = (CompoundBranch (3,(CompoundBranch (1, (SimpleBranch (1, 1), SimpleBranch (1, 1))),SimpleBranch (1, 4))),SimpleBranch (6, 3))

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


  let rec exptoint : exp * exp -> int
  = fun (exp,integer) ->
  match exp with
  |X -> exptoint (integer, integer)
  |INT n -> n
  |ADD (ex1, ex2) -> exptoint (ex1, integer) + exptoint (ex2, integer)
  |SUB (ex1, ex2) -> exptoint (ex1, integer) - exptoint (ex2, integer)
  |MUL (ex1, ex2) -> exptoint (ex1, integer) * exptoint (ex2, integer)
  |DIV (ex1, ex2) -> exptoint (ex1, integer) / exptoint (ex2, integer)


  


  let rec calculator : exp -> int
  = fun exp -> (* TODO *)
  match exp with
  |X -> raise (NotImplemented)
  |INT n -> n
  |ADD (ex1, ex2) -> exptoint (ADD (ex1, ex2), INT 0)
  |SUB (ex1, ex2) -> exptoint (SUB (ex1, ex2), INT 0)
  |MUL (ex1, ex2) -> exptoint (MUL (ex1, ex2), INT 0)
  |DIV (ex1, ex2) -> exptoint (DIV (ex1, ex2), INT 0)
  |SIGMA (int1, int2, ex3) -> if exptoint (SUB (int1, int2), INT 0) <= 0 then exptoint (ex3, int1) + calculator (SIGMA(ADD (int1, INT 1),int2, ex3)) else 0
  
  let a = ADD(INT 1, ADD(INT 2, INT 3))
  let b = SIGMA (INT 1, INT 10, MUL(X,X))

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
  = fun exp -> 
  match exp with
  | V vari -> true
  | P (vari, exp) ->
    (match exp with
    |V v -> (if vari = v then true else false)
    |C (e1,e2) -> (check (P (vari,e1))||check (P (vari,e2)))&&check e1&&check e2
    |P (v,e) -> check (P (v,e)) && check (P (vari, e))
    )
  | C (exp1, exp2) -> check exp1 && check exp2

    let a= P ("a", V "a")
    let b= P ("a", P ("a", V "a"))
    let c= P ("a", V "b")
    let d= P ("a", C (V "a", P ("b", V "c")))
    let e= P ("a", P ("b", C (V "a", V "c")))



  end