
(* problem 1*)
type btree = Empty | Node of int * btree * btree

let mirror : btree -> btree
= fun t -> let rec f t = match t with 
           | Empty -> Empty
           | Node(n, l, r) -> Node(n, f r, f l) in
           f t

(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 -> let rec f n1 n2 = match n1 with
               | SUCC(m1) -> f m1 (SUCC(n2))
               | ZERO -> n2 in
               f n1 n2

let natmul : nat -> nat -> nat 
= fun n1 n2 -> let rec f n1 n2 = 
               match n1 with
               | ZERO -> ZERO 
               | SUCC(m) -> natadd n2 (f m n2) in
               f n1 n2

let natexp : nat -> nat -> nat 
= fun n1 n2 -> let rec f n1 n2 = 
               match n2 with
               | ZERO -> SUCC(ZERO) 
               | SUCC(m) -> natmul n1 (f n1 m) in
               f n1 n2

(* problem 3
type formula =
    True
  | False
  | Var of string
  | Neg of formula
  | And of formula * formula
  | Or of formula * formula
  | Imply of formula * formula
  | Iff of formula * formula

let sat : formula -> bool
= fun f -> let rec g f = match f with 
           | Var p -> if let Var p = True in g f then True else False 
           | Neg (p) -> if g p = True then False else True
           | And (p1, p2) -> if g p1 = True && g p2 = True then True else False
           | Or (p1, p2) -> if g p1 = False && g p2 = False then False else True
           | Imply (p1, p2) -> if g p1 = True && g p2 = False then False else True
           | Iff(p1, p2) -> if (g p1 = True && g p2 = True) || (g p1 = False && g p2 = False) then True else False in
           let rec canonical lst result = match lst with
                                |[] -> result
                                |h::t -> begin match result with 
                                         |[] -> h::[]
                                         |hd::tl -> if h = hd then canonical t result else 
           let makelst f result = match f with
           | Var p -> find (Var p) result
           | Neg (p) -> if g p = True then False else True
           | And (p1, p2) -> if g p1 = True && g p2 = True then True else False
           | Or (p1, p2) -> if g p1 = False && g p2 = False then False else True
           | Imply (p1, p2) -> if g p1 = True && g p2 = False then False else True
           | Iff(p1, p2) -> if (g p1 = True && g p2 = True) || (g p1 = False && g p2 = False) then True else False in
           *)
(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) -> let rec f (e, x) = match e with
                                  | Const n -> Const 0
                                  | Var n -> if n = x then Const 1 else Const 0
                                  | Power (n, i) -> if n = x then Times [Const i; Power (n, i-1)] else Const 0
                                  | Times lst -> begin match lst with
                                                 |[] -> Const 0
                                                 |h::t -> begin match h with
                                                          | Var n -> if n = x then Const 1 else Times [h; f(Times t, x)]
                                                          | Power (n, i) -> if n = x then Times ([Const i; Power(n, i-1)]@t) else Times [h; f(Times t, x)]
                                                          | _ -> Times [h; f(Times t, x)] end 
                                                 end
                                  | Sum lst -> begin match lst with
                                               |[] -> Const 0
                                               |h::t -> Sum [f (h, x); f(Sum t, x)] end in 
              f (e, x)

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> let rec f e lst = match lst with
                         |[] -> begin match e with 
                                | X ->  0
                                | INT n -> n
                                | ADD(m, n) -> f m lst + f n lst
                                | SUB(m, n) -> f m lst - f n lst
                                | MUL(m, n) -> f m lst * f n lst
                                | DIV(m, n) -> if m = X || n = X then 0 else f m lst / f n lst 
                                | SIGMA(l, m, n) -> let a = f l lst in
                                                    let b = f m lst in
                                                    let rec makelst x y = if x>y then [] else
                                                                          (INT x)::(makelst (x+1) y) in
                                                    f n (makelst a b) end
                          |h::t -> begin match e with
                               | X -> f h []
                               | INT n -> n
                               | ADD(m, n) -> if t = [] then f m [h] + f n [h] else f m [h] + f n [h] + f e t
                               | SUB(m, n) -> if t = [] then f m [h] - f n [h] else f m [h] - f n [h] + f e t
                               | MUL(m, n) -> if t = [] then f m [h] * f n [h] else f m [h] * f n [h] + f e t
                               | DIV(m, n) -> if t = [] then f m [h] / f n [h] else f m [h] / f n [h] + f e t
                               | SIGMA(l, m, n) -> let a = f l [h] in
                                                   let b = f m [h] in
                                                   let rec makelst x y = if x>y then [] else
                                                                         (INT x)::(makelst (x+1) y) in
                                                   if t = [] then f n (makelst a b) else f n (makelst a b) + f e t end in   
                           f e []      

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m -> let rec f m = let rec weightf = fun (branchl, branchr) -> let rec weightget x = match x with 
                                                             |SimpleBranch(l, w) -> w
                                                             |CompoundBranch(l, m) -> weightf m in
                                                   weightget branchl + weightget branchr in 
           match m with 
           |(SimpleBranch(ll, wl), SimpleBranch(lr, wr)) -> if ll * wl == lr * wr then true else false
           |(CompoundBranch(ll, ml), CompoundBranch(lr, mr)) -> if f ml == true && f mr == true && ll * weightf ml == lr * weightf mr then true else false
           |(SimpleBranch(ll, wl), CompoundBranch(lr, mr)) -> if f mr == true && ll * wl == lr * weightf mr then true else false
           |(CompoundBranch(ll, ml), SimpleBranch(lr, wr)) -> if f ml == true && ll * weightf ml == lr * wr then true else false in
           f m 

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 -> let rec frev x rev = match x with 
                                    |[] -> rev 
                                    |h::t -> frev t (h::rev) in
               let rec bsum l1 l2 = match l1 with
                                    |[] -> [ZERO](**)
                                    |h1::t1 -> match l2 with
                                               |[] -> [ZERO](**)
                                               |h2::t2 -> if (h1 == ZERO) && (h2 == ZERO) then if t1 == [] && t2 == [] then [ZERO] else
                                                                                               if t1 == [] then ZERO::t2 else
                                                                                               if t2 == [] then ZERO::t1 else
                                                                                               ZERO::(bsum t1 t2) else
                                                          if (h1 == ZERO && h2 == ONE) || (h1 == ONE && h2 == ZERO) then if t1 == [] && t2 == [] then [ONE] else
                                                                                                                         if t1 == [] then ONE::t2 else
                                                                                                                         if t2 == [] then ONE::t1 else
                                                                                                                         ONE::(bsum t1 t2) else
                                                          if t1 == [] && t2 == [] then [ZERO; ONE] else
                                                          if t1 == [] then ZERO::(bsum [ONE] t2) else
                                                          if t2 == [] then ZERO::(bsum [ONE] t1) else
                                                          ZERO::(bsum [ONE] (bsum t1 t2)) in
	       let rec f result l1 l2 = match l1 with
                                        |[] -> [ZERO](**)
                                        |h1::t1 -> if h1 == ZERO then if t1 != [] then f result t1 (ZERO::l2) else
                                                                      result else
                                                   if h1 == ONE then if t1 != [] then f (bsum result l2) t1 (ZERO::l2) else
                                                                     bsum result l2 else [ZERO](**) in 
               let b1rev = frev b1 [] in
               let b2rev = frev b2 [] in
               frev (f [ZERO] b1rev b2rev) []
