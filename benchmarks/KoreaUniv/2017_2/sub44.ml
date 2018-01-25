(* problem 1*)
type btree = Empty | Node of int * btree * btree

let rec mirror : btree -> btree
= fun t ->
  match t with
  |Empty -> Empty
  |Node (a,b,c) -> Node (a,(mirror c),(mirror b));;

(* problem 2*)
type nat = ZERO | SUCC of nat

let natadd : nat -> nat -> nat 
= fun n1 n2 ->
  let rec plus a1 a2 =
    match a1 with
    |ZERO -> (match a2 with
             |ZERO -> ZERO
             |SUCC a -> SUCC (plus a1 a))
    |SUCC b -> SUCC(plus b a2)
  in plus n1 n2;;

let natmul : nat -> nat -> nat 
= fun n1 n2 ->
  let rec mul a1 =
    match a1 with
    |ZERO -> ZERO
    |SUCC a -> natadd n2 (mul a)
  in mul n1;;

let natexp : nat -> nat -> nat 
= fun n1 n2 ->
  let rec exp a1 =
    match a1 with
    |ZERO -> SUCC ZERO
    |SUCC a -> natmul n1 (exp a)
  in exp n2;;

(* problem 3*)
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
= fun f ->
  let vars =
    let rec findval form vars =
      match form with
      |True -> []
      |False -> []
      |Var a -> if (let rec search mylist =
                    match mylist with
                    |[] -> false
                    |hd::tl -> if a = hd then true else search tl
                  in search vars ) then vars else a::vars
      |Neg a -> findval a vars
      |And (a,b) -> findval a (findval b vars)
      |Or (a,b) -> findval a (findval b vars)
      |Imply (a,b) -> findval a (findval b vars)
      |Iff (a,b) -> findval a (findval b vars)
    in findval f []
    in let rec check form l =
      match form with
      |True -> true
      |False -> false
      |Var a -> (let rec find mylist =
                  match mylist with
                  |hd::tl -> (match hd with
                            |(c,d) -> if c = a then d else find tl)
                  |[] -> false
                in find l)
      |Neg a -> not (check a l)
      |And (a,b) -> (check a l) && (check b l)
      |Or (a,b) -> (check a l) || (check b l)
      |Imply (a,b) -> (not (check a l)) || (check b l)
      |Iff (a,b) -> let myiff c d = (not c || d) && (c || not d) in myiff (check a l) (check b l)
    in let rec truth mylist cur = 
      match mylist with
      |hd::tl -> (truth tl ((hd, true )::cur)) || (truth tl ((hd, false)::cur))
      |[] -> check f cur 
    in truth vars [];;

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let diff : aexp * string -> aexp
= fun (e,x) ->
  let rec ae f isfactor =
    match f with
    |Sum a -> let rec sumhelp b =
                match b with
                |[] -> []
                |hd::tl -> (ae hd false)::(sumhelp tl)
              in Sum (sumhelp a)
    |Times a -> let isx = 
                  let rec xsearch term = 
                    match term with
                    |[] -> false
                    |hd::tl -> match hd with
                              |Var a -> if a = x then true
                                        else xsearch tl
                              |Power (a,b) -> if a = x then
                                              (if b = 0 then xsearch tl
                                                else true)
                                              else xsearch tl
                              |_ -> xsearch tl
                  in xsearch a
                in let rec timeshelp b =
                    match b with
                    |[] -> []
                    |hd::tl -> (ae hd isx)::(timeshelp tl)
                  in Times (timeshelp a)
    |Power (a,b) -> if a <> x then if isfactor then Power (a,b) else Const 0
                  else if b = 1 then Const 1
                  else if b = 0 then if isfactor then Const 1 else Const 0
                  else Times [(Const b); (Power (a,(b-1)))]
    |Var a -> if a = x then Const 1
              else if isfactor then Var a 
              else Const 0
    |Const a -> if isfactor then Const a 
                else Const 0
    in ae e false;;

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let calculator : exp -> int
= fun e -> 
    let rec calc e cur =
      match e with
      |X -> cur
      |INT a -> a
      |ADD (a,b) -> (calc a cur) + (calc b cur)
      |SUB (a,b) -> (calc a cur) - (calc b cur)
      |MUL (a,b) -> (calc a cur) * (calc b cur)
      |DIV (a,b) -> (calc a cur) / (calc b cur)
      |SIGMA (a,b,c) -> let rec sigm bot =
                          if bot = calc b 0 then calc c bot
                          else (calc c bot) + sigm (bot+1)
                        in sigm (calc a 0)
    in calc e 0;;

(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let balanced : mobile -> bool
= fun m ->
  let rec wei mob =
    match mob with
    |(left,right) -> (match left with
                      |SimpleBranch (a,b) -> (match right with
                                              |SimpleBranch (c,d) -> b+d
                                              |CompoundBranch (c,d) -> b+(wei d))
                      |CompoundBranch (a,b) -> (match right with
                                                |SimpleBranch (c,d) -> d + (wei b)
                                                |CompoundBranch (c,d) -> (wei d) + (wei b)))
  in let rec balhelp mob =
    match mob with
    |(left,right) -> (match left with
                      |SimpleBranch (a,b) -> (match right with
                                              |SimpleBranch (c,d) -> if a*b = c*d then true else false
                                              |CompoundBranch (c,d) -> if a*b = c*wei d && balhelp d then true else false)
                      |CompoundBranch (a,b) -> (match right with
                                                |SimpleBranch (c,d) -> if c*d = a*wei b && balhelp b then true else false
                                                |CompoundBranch (c,d) -> if c*wei d = a*wei b && balhelp d && balhelp b then true else false))
    in balhelp m;;

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let bmul : bin -> bin -> bin
= fun b1 b2 ->
  let rec createdec b pot =
    match b with
    |[] -> 0
    |hd::tl -> (match hd with
                |ZERO -> createdec tl (pot*2)
                |ONE -> (1*pot)+(createdec tl (pot*2)))
  in let res = (createdec (List.rev b1) 1) * (createdec (List.rev b2) 1)
  in if res = 0 then [ZERO] else 
    let rec createbin b =
        match b with
        |0 -> []
        |_ -> (createbin (b/2))@(if b mod 2 = 0 then [ZERO] else [ONE])
  in createbin res;;


