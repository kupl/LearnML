(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst =
match 1st with
	| [] -> []
	| hd::tl -> if (pred hd) then hd::(filter pred tl) else (filter pred tl);;

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) ->
let rec insert x 1 =
	match 1 with
	| [] -> [x]
	| hd::t1 -> if x < hd then x::hd::tl
		else hd::insert x t1

let rec zipper ((a:int list),(b: int list)) =
	match a with 
	| [] -> b
	| hd::t1 -> insert hd (zipper (t1,b));;

zipper ([1;3;5],[2;4;6]);;
zipper ([1;3],[2;4;6;8]);;
zipper ([1;3;5;7],[2;4]);;


(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> 
match n with
	|0 -> (function x->x)
	|1 -> f
	|_ -> function x -> f ((iter(n-1),f))x)

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

type aexp =

        | Const of int

        | Var of string

        | Power of string * int

        | Times of aexp list

        | Sum of aexp list

 

let rec diff (e, v) = match e with

        | Const n -> Const 0

        | Var x -> if (x =v) then Const 1 else Var x

        | Power(x,i) -> if(i==1) then Const 1

                         else Times[Const i;Power(x,i-1)]

        | Times al -> let rec diff_times el x = match el with

                           | [] -> []

                           | (Const c as hd)::tl -> hd::(diff_times tl x)

                           | (Times ts) ::tl -> diff_times (List.append ts tl) x

                           | hd::tl -> (diff (hd, v))::(diff_times tl x)

                            in let rec clean_times rs coef = match rs with

                                | []->[],coef

                                | (Const n)::tl -> clean_times tl (coef*n)

                                | hd::tl -> let t0,m = clean_times tl coef

                                             in hd::t0,m

                                in let rec concat_times ts = match ts with

                            | []->[]

                            | (Times t1)::tl -> concat_times (List.append t1 tl)

                            | hd::tl -> hd::(concat_times tl)

                              in

                              let t1 =diff_times al v in

                              let t2 = concat_times t1 in

                              let t3,n = clean_times t2 1 in

                                if n=1 then Times t3 else Times ((Const n)::t3)

 

        | Sum al -> let rec diff_list el x = match el with

                                    | [] -> []

                                    | hd::tl  -> (diff (hd, x))::(diff_list tl x)

                                     in

                                     let rec clean_sum rs con = match rs with

                                    | []->[],con

                                    | (Times [a])::tl -> clean_sum (a::tl) con

                                    | (Const n)::tl -> clean_sum tl (con + n)

                                    | hd::tl -> let ts,con1 = clean_sum tl con in                                        (hd::ts),con1

                                       in

                                       let s1 = diff_list al v in

                                       let s2,n= clean_sum s1 0 in

                                       if n =0 then Sum s2

                                       else Sum(List.append s2 [Const n])

;;

diff (Sum[Power("x",2);Times[Const 2; Var "x"]; Const 1],  "x");;

diff (Sum[Times[Const 2;Power("x",3)];Times[Const 2; Var "y"];Const 3],"x");;

diff (Sum[Times[Const 3; Var "x"]; Times[Const 2; Var "x"]],"x");;

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
=fun e -> 0 (* TODO *)
