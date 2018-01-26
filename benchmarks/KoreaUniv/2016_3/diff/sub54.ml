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

let rec map f l = match l with 
   |[] -> []
   |hd::tl -> (f hd)::(map f tl)

    let rec diff : aexp * string -> aexp
    = fun (exp, var) -> match exp with
    |Const n -> Const 0
    |Var x -> if x = var then Const 1 else Const 0
    |Power(x,n) ->if x=var then Times[Const n; Power(x,n-1)] else Const 0
    |Times [] -> Const 0
    |Times(hd::tl) -> Sum [Times (diff(hd,var)::tl);Times [hd;diff(Times tl,var)]]
    |Sum aexps -> Sum (map(fun ae -> diff(ae,var)) aexps)         
    |_ -> raise NotImplemented 

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
  

let rec evalWeight : branch -> weight
    = fun bran ->
     match bran with
    |  SimpleBranch (l,w) -> w
    | CompoundBranch (l,m) ->
        (match m with
          (lb,rb) -> (evalWeight lb) + (evalWeight rb));;
  
    let rec evalTorque : branch -> int
    = fun bran ->
      match bran with
     | SimpleBranch (l,w) -> l * w
    | CompoundBranch (l,m) ->
        (match m with
          (lb,rb) -> l * (evalWeight lb + evalWeight rb))

        let rec balanced : mobile -> bool
            = fun (leftB,rightB) ->
              match leftB,rightB with
            |  SimpleBranch (l1,w1),SimpleBranch (l2,w2) ->
                if evalTorque leftB = evalTorque rightB then true else false
            | SimpleBranch (l1,w),CompoundBranch (l2,m) ->
                if (balanced m = true) && (evalTorque leftB = evalTorque rightB)
                  then true else false
            | CompoundBranch (l1,m),SimpleBranch (l2,w) ->
                if (balanced m = true) && (evalTorque leftB = evalTorque rightB)
                  then true else false
            | CompoundBranch (l1,m1),CompoundBranch (l2,m2) ->
                if (balanced m1 = true) && (balanced m2 = true)
                && (evalTorque leftB = evalTorque rightB)
                  then true else false

            |_ ->raise NotImplemented 
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


   let rec map f l = match l with 
   |[] -> []
   |hd::tl -> (f hd)::(map f tl)


   let rec range: int -> int -> int list = fun n1 n2 ->
   if n1 = n2 then [n1] else n1::(range (n1+1) n2)

   let rec sum: int list -> int = fun l -> match l with
   |[] -> 0
   |hd::tl -> hd + (sum tl)

   let rec evalExp : exp -> int list -> int = fun e env ->
   match e with
   |X -> (match env with
   	|[] -> raise NotImplemented
   	|hd::tl -> hd)
   |INT n -> n
   |ADD(e1, e2) -> (evalExp e1 env) + (evalExp e2 env)
   |SUB(e1,e2) -> (evalExp e1 env)-(evalExp e2 env)
   |MUL(e1,e2) ->(evalExp e1 env)*(evalExp e2 env)
   |DIV(e1,e2) -> (evalExp e1 env)/(evalExp e2 env)
   |SIGMA(e1,e2,e3) -> 
   let n1 = (evalExp e1 env) in
   let n2 = (evalExp e2 env) in
   sum (map (fun n -> evalExp e3 [n]) (range n1 n2))

  let rec calculator : exp -> int = fun exp ->  let env = [] in 
   match exp with
   |X -> (match env with
   	|[] -> raise NotImplemented
   	|hd::tl -> hd)
   |ADD(e1, e2) -> (evalExp e1 env) + (evalExp e2 env)
   |SUB(e1,e2) -> (evalExp e1 env)-(evalExp e2 env)
   |MUL(e1,e2) ->(evalExp e1 env)*(evalExp e2 env)
   |DIV(e1,e2) -> (evalExp e1 env)/(evalExp e2 env)
   |INT n -> n
   |SIGMA(e1,e2,e3) -> 
   let n1 = (evalExp e1 env) in
   let n2 = (evalExp e2 env) in
    sum (map (fun n -> evalExp e3 [n]) (range n1 n2))

end

    
(*     Problem 4     *)
(*********************)
module Problem4 = struct
  type exp =
  | V of var
  | P of var * exp
  | C of exp * exp
  and var = string

let rec varOfExp : exp -> var = fun exp ->
   match exp with
   |V var -> var
   |P(var,e) -> varOfExp(e)
   |C(e1,e2) -> varOfExp(e2)


   let check : exp -> bool
  = fun exp -> match exp with
  |P(var1,P(var2,e)) -> if (varOfExp e = var2) || (varOfExp e = var1) then true else false
  |P(var1,C(e1,e2)) -> if (varOfExp e2 = var1) then true else false
  |P(var1,V var2) -> if var1= var2 then true else false
  |_ -> raise NotImplemented
end

