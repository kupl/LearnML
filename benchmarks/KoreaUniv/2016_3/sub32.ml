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
     | Const c -> Const 0
     | Var x -> if x=var then Const 1 else Const 0
     | Power (x, c) -> if x=var then 
                   (if c=1 then Const 1 else Times [Const c; Power (x, c-1)])
                    else Const 0
     | Times lst -> (match lst with
                     | [] -> Const 1
                     | hd::tl -> if tl=[] then diff (hd, var) 
      else Sum [Times[(diff (hd, var)); Times tl]; Times[hd; (diff (Times tl, var))]])
     | Sum lst -> (match lst with
                   | [] -> Const 0
                   | hd::tl -> if tl=[] then diff (hd, var) 
      else Sum [(diff(hd, var)); (diff(Sum tl, var))]);;
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

let rec m_weight : branch -> weight
= fun br ->
         match br with
         | SimpleBranch (l, w) -> w
         | CompoundBranch (l, m) -> (match m with
                                      (lb, rb) -> (m_weight lb) + (m_weight rb));;

let rec m_torque : branch -> int
= fun t ->
         match t with
         | SimpleBranch (l, w) -> l * w
         | CompoundBranch (l, m) -> (match m with
                                     (lb, rb) -> if (m_torque lb) = (m_torque rb)
                                                then (l * ((m_weight lb) + (m_weight rb)))
                                                else (0));;

let balanced : mobile -> bool
= fun mob -> 
         match mob with
         (lb, rb) -> if(m_torque lb) = (m_torque rb) then true else false;;
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

  let calculator : exp -> int
  = fun exp -> raise NotImplemented

let rec sigma : int * int * (int -> int) -> int
= fun (n1, n2, f) -> 
          if (n1 > n2) then raise (Failure "error") else if (n1 = n2) then f n1
          else (f n1) + (sigma (n1 + 1, n2, f));;

let rec xcal : exp -> (int -> int)
= fun exp ->
   match exp with
   | X -> (fun x-> x)
   | INT n -> (fun x-> n)
   | ADD (e1, e2) -> (fun x -> (((xcal e1) x) + ((xcal e2) x)))
   | SUB (e1, e2) -> (fun x -> (((xcal e1) x) - ((xcal e2) x)))
   | MUL (e1, e2) -> (fun x -> (((xcal e1) x) * ((xcal e2) x)))
   | DIV (e1, e2) -> (fun x -> (((xcal e1) x) / ((xcal e2) x)))
   | SIGMA (e1, e2, e3) -> raise (Failure "impossible");;

let rec cal : exp -> int
= fun exp -> 
   match exp with
   | X -> raise (Failure "nonevalue")
   | INT n -> n
   | ADD (e1, e2) -> (cal e1) + (cal e2)
   | SUB (e1, e2) -> (cal e1) - (cal e2)
   | MUL (e1, e2) -> (cal e1) * (cal e2)
   | DIV (e1, e2) -> if (cal e2) = 0 then raise (Failure "divide by zero") 
                     else (cal e1) / (cal e2)
   | SIGMA (e1, e2, e3) -> sigma (cal e1, cal e2, (xcal e3));;

let calculator : exp -> int
= fun exp -> 
   match exp with
   | X -> 0
   | INT n -> n
   | ADD (e1, e2) -> (cal (ADD (e1, e2)))
   | SUB (e1, e2) -> (cal (SUB (e1, e2)))
   | MUL (e1, e2) -> (cal (MUL (e1, e2)))
   | DIV (e1, e2) -> (cal (DIV (e1, e2)))
   | SIGMA (e1, e2, e3) -> (cal (SIGMA (e1, e2, e3)));;
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

let rec check_val : exp * (var list) -> bool
= fun (exp, lst) ->
   match exp with
   | V x -> (match lst with
             | [] -> false
             | hd::tl -> if x=hd then true else check_val(V x, tl))
   | P (x, e1) -> check_val(e1, [x]@lst)
   | C (e1, e2) -> if (check_val(e1, lst)=true && check_val(e2, lst)=true) 
                   then true else false;;

let check : exp -> bool
= fun exp -> check_val (exp, []);;
end

