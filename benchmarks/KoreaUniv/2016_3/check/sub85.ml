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
  = fun (exp, var) -> (* TODO *)
    match exp with
    | Const _ -> Const 0
    | Var k -> if var = k then Const 1 else Const 0
    | Power (k,m) ->  if not (var = k) then Const 0
                      else Times [Const m; Power (k, m-1)]
    | Times [] -> Const 0
    | Times (head::tail) -> Sum [Times (diff (head,var)::tail); Times [head; diff(Times tail, var)]]
    | Sum aexps -> Sum (List.map (fun dum -> diff (dum,var)) aexps);;
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

  let rec get_PW tree = 
          match tree with
          | SimpleBranch(len, wet) -> wet 
          | CompoundBranch(len, mob) ->
                   match mob with 
                   | (m,n) -> (get_PW m) + (get_PW n);;
  let rec get_W tree = 
          match tree with
          | SimpleBranch(len, wet) -> len*wet 
          | CompoundBranch(len, mob) ->
                    match mob with (m,n) -> 
                    if (get_W n)=(get_W m) then ((get_PW m)+(get_PW n))*len  else (-1);;

  let balanced : mobile -> bool
  = fun mob -> (*raise NotImplemented *)
      match mob with (left_Ch, right_Ch) -> if (get_W left_Ch)=(get_W right_Ch) then true else false;;
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

  let rec calculator : exp -> int = fun exp -> (* raise NotImplemented*)  (* TODO *)
    let rec get_value parameter =
      begin 
        match parameter with
      | X -> raise NotImplemented  
      | INT n -> n 
      | ADD (n, m) -> (get_value n) + (get_value m) 
      | SUB (n, m) -> (get_value n) - (get_value m) 
      | MUL (n, m) -> (get_value n) * (get_value m) 
      | DIV (n, m) -> (get_value n) / (get_value m) 
      | SIGMA (n, m, ex) ->  
                let rec getexint k =
                  begin
                    match k with
                   | X -> (fun para->para) 
                   | INT n -> (fun para->n) 
                   | ADD (m,n) -> (fun para-> ((getexint m) para) + ((getexint n) para) ) 
                   | SUB (m,n) -> (fun para-> ((getexint m) para) - ((getexint n) para) )  
                   | MUL (m,n) -> (fun para-> ((getexint m) para) * ((getexint n) para) ) 
                   | DIV (m,n) -> (fun para-> ((getexint m) para) / ((getexint n) para) ) 
                   | SIGMA (m,n,r) -> raise NotImplemented
                  end
                 in
                 let rec getScope v1 v2 =
                   if v1>v2 then raise NotImplemented
                   else if v1=v2 then ((getexint ex) v2) 
                   else ( ((getexint ex) v1) + (getScope (1+v1) v2))
                 in (getScope (get_value n) (get_value m))
      end
    in match exp with
    | X -> 0 
    | INT n -> n
    | ADD (n,m) -> (get_value (ADD (n,m))) 
    | SUB (n,m) -> (get_value (SUB (n,m))) 
    | MUL (n,m) -> (get_value (MUL (n,m))) 
    | DIV (n,m) -> (get_value (DIV (n,m))) 
    | SIGMA (n,m,eex) -> (get_value (SIGMA (n,m,eex)));;
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

  let rec evaluate_vpc expression vpc =
    match expression with
    |V a -> let rec check list st = 
          match list with      
          [] -> false 
          | head::tail -> if head=st then true else (check tail st) in if (check vpc a) then true else false 
    | P (pa, ex1) -> if ( evaluate_vpc ex1 (vpc @ [pa]) ) then true else false
    | C (ex1, ex2) -> if (evaluate_vpc ex1 vpc && evaluate_vpc ex2 vpc) then true else false;;

  let check : exp -> bool
  = fun exp -> evaluate_vpc exp [];; (* raise NotImplemented *) (* TODO *)
end

