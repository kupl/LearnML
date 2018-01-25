(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = match lst with
[] -> []
| h::t -> if pred h then h::(filter pred t) else filter pred t

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> match a with 
[]-> b
| h1::t1 -> h1::(match b with 
          [] -> t1
          | h2::t2 ->  h2::(zipper (t1, t2))
        )
(*******************)
(* Problem 3: iter *)
(*******************)
let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> match n with
0 -> fun x-> x
|_ -> fun x-> f (iter(n-1, f) x)

(*********************)
(* Problem 4: Diff   *)
(*********************)
type aexp = 
  | Const of int 
  | Var of string 
  | Power of string * int 
  | Times of aexp list
  | Sum of aexp list

(*let rec diff_cln aexp = *)
let rec diff : aexp * string -> aexp
=fun (aexp,x) -> match aexp with
Const i -> Const 0
| Var v -> if v=x then Const 1 else Var v
| Power (v,p) ->if v=x then match p with
                 1 ->  Const 1
                |2 ->  Times [Const p; Var v]
                |_ ->  Times [Const p; Power (v, p-1)]
              else Power (v,p)
| Times l1 -> let rec iter l= match l with
              [] -> [] 
              | h::t -> let rec isConst l = match l with 
                    [] -> true (*if this continues, it reaches here*)
                    | h::t -> match h with 
                          Const c -> isConst t  
                    | _ -> false
                  in if isConst l then [Const 0] else 
              match h with 
                        Const c -> h::iter(t)
                       | Sum lx -> h::iter(t)
                       | _ -> diff(h,x)::iter(t)
              in Times (iter l1)  
| Sum l2 -> let rec iter l = match l with
              [] -> [] 
              | h::t -> match h with 
                    Const 0 -> iter(t)
                  |  Times [Const 0] -> iter(t)
                  | _ -> diff(h,x)::iter(t)
              in Sum (iter l2)
(* in match aexp.....

example

diff (Sum [Power ("x", 2); Times [Const 2; Var "x"]; Const 1],"x")
diff (Sum [Times [Const 2; Var "x"]; Times [Const 2; Const 1]; Const 0],"x")
diff (Sum [Power ("x", 4); Times [Const 3; Power ("x",2)]; Const 4],"x")
diff (Sum [Times [Sum [Const 1; Var "y"]; Power ("x", 5)]],"x")
diff (Sum
 [Times [Const 4; Power ("x", 3)]; Times [Const 3; Times [Const 2; Var "x"]];
  Const 0],"x")
diff (Sum
 [Times [Const 4; Times [Const 3; Power ("x", 2)]];
  Times [Const 3; Times [Const 2; Const 1]]]
,"x")
diff (Sum
 [Times [Const 4; Times [Const 3; Times [Const 2; Var "x"]]];
  Times [Const 3; Times [Const 0]]]
,"x")
*)

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
=fun e -> let rec calc : exp * exp -> int 
=fun (v, ee)-> match ee with 
X -> calc(INT 0, v)
| INT(i) -> i
| ADD (e1,e2) -> calc(v,e1)+calc(v,e2)
| SUB (e1,e2) -> calc(v,e1)-calc(v,e2)
| MUL (e1,e2) -> calc(v,e1)*calc(v,e2)
| DIV (e1,e2) -> calc(v,e1)/calc(v,e2)
| SIGMA (i,j,e3) -> if i=j then calc(i,e3) else calc(v, (ADD(INT (calc(i,e3)), SIGMA( (INT (calc(INT 0,i)+1)),j,e3))))
in calc (INT 0,e)



(*calculator (SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1)));;*)
