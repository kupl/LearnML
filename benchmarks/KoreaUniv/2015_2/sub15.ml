(*********************)
(* Problem 1: filter *)
(*********************)
let rec filter pred lst = [] 
(module LinkedList = 
 let filter pred list = 
         let folder head tail =
            if pred head then 
                Cons(head,tail)
            else
                tail)

(*********************)
(* Problem 2: zipper *)
(*********************)
let rec zipper : int list * int list -> int list
=fun (a,b) -> [] 
(let rec zip (xs : int list) (ys : int list) : (int * int) list option =
  match (xs,ys) with
    ([], []) -> Some []
  | (x::xtail,[]) -> None
  | ([],y::ytail) -> None
  | (x::xtail,y::ytail) -> 
      (match zip xtail ytail with
         None -> None
       | Some zs -> Some ((x,y) :: zs))
;;
)

(*******************)
(* Problem 3: iter *)
(*******************)

let rec iter : int * (int -> int) -> (int -> int)
=fun (n,f) -> f 
(let rec iter f a n = 
if (n = 0) then 
a else 
(f n (iter f a (n-1)))

)

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
(type aexp = Num of int
    | Var of string
    | Plus of aexp * aexp
    | Minus of aexp * aexp
    | Product of aexp * aexp;;
let update s x n = function var -> if x = var then Some n else s var;;
)

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
