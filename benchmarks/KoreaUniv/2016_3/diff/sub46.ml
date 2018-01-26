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
  = fun (exp, var) -> (* raise NotImplemented (* TODO *) *)
      match exp with
      | Const a -> Const 0
      | Var s   -> if s=var then Const 1 else Const 0
      | Power (s,n) -> if s=var then Times[Const n; Power(s,n-1)] else Const 0     
      | Times lst -> (match lst with
                    | []    -> Const 1   
                    | h::[] -> diff(h,var)            
                    | h::t  -> (match h with
                                | Const 0 -> Const 0
                                | Const 1 -> diff (Times t,var)
                                | Const n -> Times[Const n; diff (Times t,var)]
                                | _ -> Sum [Times(diff(h,var)::t); Times[h;diff(Times t,var)]]))
      | Sum lst2 -> (match lst2 with
                    | []    -> Const 0
                    | h::[] -> diff(h,var)
                    | h::t  -> Sum [ (diff(h,var)) ; (diff(Sum t,var)) ])
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

  let rec weight branch =
    match branch with
    | SimpleBranch (l,w) -> w
    | CompoundBranch (l,m) ->
      (match m with (a,b) -> weight a + weight b)

  let rec totalweight branch =
    match branch with
    | SimpleBranch (l,w) -> l*w
    | CompoundBranch (l,m) ->
        match m with (a,b) ->
          (
          if (totalweight a) = (totalweight b) then ((weight a) + (weight b))*l
          else -9797
          )


  let balanced : mobile -> bool
  = fun mob -> (* raise NotImplemented (* TODO *) *)
    match mob with (left, right) ->
      if (totalweight left) = (totalweight right) then true else false
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

  let rec sigma : int * int * (int->int) -> int
  = fun (n1, n2, func) ->
    if n1 > n2 then 0
    else if n1 = n2 then func n1
    else (func n1) + (sigma (n1+1,n2,func));; 

  let rec etof : exp -> (int -> int)
  = fun e ->
      match e with
  | X -> (fun x -> x)
  | INT n -> (fun x -> n)
  | ADD (exp1, exp2) -> (fun x -> ((etof exp1) x) + ((etof exp2) x))
  | SUB (exp1, exp2) -> (fun x -> ((etof exp1) x) - ((etof exp2) x))
  | MUL (exp1, exp2) -> (fun x -> ((etof exp1) x) * ((etof exp2) x))
  | DIV (exp1, exp2) -> (fun x -> ((etof exp1) x) / ((etof exp2) x))
  | SIGMA (exp1, exp2, exp3) -> (fun x -> sigma( (etof exp1) x, (etof exp2)x, (etof exp3)));;

  let rec calculator : exp -> int
  = fun exp -> match exp with
    | X -> raise (Failure "error : free-variable")
    | INT a -> a
    | ADD (INT n1, INT n2) -> n1 + n2
    | ADD (exp1, exp2) -> (calculator exp1) + (calculator exp2)
    | SUB (INT n1, INT n2) -> n1 - n2
    | SUB (exp1, exp2) -> (calculator exp1) - (calculator exp2)
    | MUL (INT n1, INT n2) -> n1 * n2
    | MUL (exp1, exp2) -> (calculator exp1) * (calculator exp2)
    | DIV (INT n1, INT n2) -> if n2 = 0 then raise (Failure "DIVISION ERROR") else n1/n2
    | DIV (exp1, exp2) -> (calculator exp1) / (calculator exp2)
    | SIGMA (INT n1, INT n2, exp1) -> sigma (n1, n2, (etof exp1))
    | SIGMA (exp1, exp2, exp3) -> sigma (calculator exp1, calculator exp2, etof exp3);;
  
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

  let rec expToVar : exp -> var
  = fun exp ->
      match exp with
      |V var        -> var
      |P (var,ex)   -> expToVar(ex)
      |C (ex1,ex2)  -> expToVar(ex2)

  let rec check : exp -> bool
  = fun exp -> 
    let rec pcheck : exp * (var list) -> bool
    = fun (aexp, vlist) -> 
      match aexp with
        V s -> (match vlist with
          [] -> false
          | h::t -> if s = h then true else pcheck (V s, t))
      | P (v,exp) -> pcheck (exp, v::vlist)
      | C (exp1, exp2) -> (pcheck (exp1,vlist)) && (pcheck (exp2,vlist))            
    in 
  pcheck (exp,[]);;

end












