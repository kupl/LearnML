(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = []
let rec filter pred lst =
  match lst with
  |[q] -> if pred q then [q] else []
  |h::t -> if pred h then h::(filter pred t) else filter pred t

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> []
let rec zipper (l1,l2) = 
  match l1 with
  |[]->l2
  |h1::t1 ->
    match l2 with
      |[]->l1
      |[h2]->if h1+0<h2+0 then zipper ([],([h1;h2])) else [h2]@(zipper (l1,[]))
      |h2::t2 -> if h1+0<h2+0 then zipper (t1,([h1]@([h2]@t2))) else [h2]@(zipper (l1,t2))

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f
let rec iter (n,f) = if n=0 then (fun x->x+0) else (fun x->f(iter(n-1,f) x));;
  

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
=fun (aexp,x) -> aexp
let rec diff(sic,v) =
  match sic with
    | Const n -> Const 0
    | Var x -> if x=v then Const 1 else Const 0
    | Power(x,n) -> if x=v then
              if n=0 then Const 0
              else if n=1 then Const 1
              else Times [Const n;Power(x,n-1)] 
            else Const 0
    | Times l ->if List.mem (Const 0) l then Const 0
          else (match l with
            | h::[] -> diff(h,v);
            | h::t -> match h with
                  | Const n -> Times[h;diff(Times t,v)]
                  | _ -> Sum [Times [diff(h,v);Times t];Times [h;diff(Times t,v)]])
    | Sum l ->(match l with
          | h::[] -> diff(h,v);
          | h::t -> Sum [diff(h,v);diff(Sum t,v)]);;

(*************************)
(* Problem 5: Calculator *)
(*************************)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp
let calculator : exp -> int
=fun e -> 0



let rec calx : exp * exp -> int
=fun (i,e) ->(
  match e with
  | X -> calx(i,i)
  | INT(n) -> n
  | ADD(q,w) -> calx(i,q)+calx(i,w)
  | SUB(q,w) -> calx(i,q)-calx(i,w)
  | MUL(q,w) -> calx(i,q)*calx(i,w)
  | DIV(q,w) -> calx(i,q)/calx(i,w))

let rec sigma : exp*exp*exp -> int
=fun (q,w,r) -> (
  if calx(q,q)<calx(w,w) then calx(q,r)+sigma(INT(calx(q,q)+1),w,r)
  else calx(q,r))

let rec cal e =
  match e with
  | INT(n) -> n
  | ADD(q,w) -> cal(q)+cal(w)
  | SUB(q,w) -> cal(q)-cal(w)
  | MUL(q,w) -> cal(q)*cal(w)
  | DIV(q,w) -> cal(q)/cal(w)
  | SIGMA(q,w,r) -> match r with |SIGMA(a,s,d) -> (cal(w)-cal(q)+1)*sigma(a,s,d) | _ -> sigma(q,w,r)

let calculator e = cal(e)