(*
완료 : 1, 2, 6, 7
요수정 : 3, 4, 5
*)

(* problem 1*)

type btree = Empty | Node of int * btree * btree
let rec mirror : btree -> btree
= fun t ->
    match t with 
        Empty -> Empty
        | Node(a, b, c) -> Node(a, (mirror c), (mirror b));;

(* problem 2*)
type nat = ZERO | SUCC of nat

let rec natadd : nat -> nat -> nat 
= fun n1 n2 -> 
    match n2 with 
        ZERO-> n1
        | SUCC(n2minus1) -> natadd (SUCC(n1)) n2minus1
;;

let rec natmul : nat -> nat -> nat 
= fun n1 n2 -> 
    match n2 with
        ZERO -> ZERO
        | SUCC(n2minus1) -> natadd n1 (natmul n1 n2minus1)
;;

let rec natexp : nat -> nat -> nat 
= fun n1 n2 -> 
    match n2 with
        ZERO -> SUCC ZERO
        | SUCC(n2minus1) -> natmul n1 (natexp n1 n2minus1)
;;

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

let rec sat : formula -> bool
= fun f ->  
(*Var 뽑아서 (Var, true) 리스트 만들기*)
    let rec makeList : formula -> (string*bool) list -> (string*bool) list
    = fun f tab ->
        match f with
        | True -> tab
        | False -> tab
        | Var x -> 
            let rec findVar var l =
                match l with
                | [] -> ((var, true) :: tab)
                | hd :: tl -> if ( hd = (var, true)) then tab else findVar var tl
            in findVar x tab
            (*in findVar x [] []*)
        | Neg x -> (makeList x tab)
        | And (x, y)-> (makeList y (makeList x tab))
        | Or (x, y) -> (makeList y (makeList x tab))
        | Imply (x, y) -> (makeList y (makeList x tab))
        | Iff (x, y) -> (makeList y (makeList x tab))
    in
    (*리스트에서 튜플 뽑아서 bool 뒤집기*)
    let rec table boolLists =
        match boolLists with
        |[]->[]
        |(a, b)::tl ->  if b = true then ((a, false):: tl) else ((a, true)::(table tl))
    in
    (*sat 확인하기*)
    let rec satTF f bulist va = 
        match f with
        | True -> if va then true else false
        | False -> if va then false else true
        | Var x -> 
            let rec matchVar v tabl =
                match tabl with
                | hd :: tl -> (
                    if (hd = (v, va)) then true
                    else if (hd = (v, not(va))) then false
                    else matchVar v tl
                )
                | [] -> raise(Failure "Something wrong")
            in matchVar x bulist
        | Neg x -> satTF x bulist (not(va))
        | And (x, y)-> 
            if va = true then ((satTF x bulist true) && (satTF y bulist true))
            else (((satTF x bulist true) && (satTF x bulist false)) || ((satTF x bulist false) && (satTF x bulist true)) || ((satTF x bulist false) && (satTF x bulist false)))
        | Or (x, y) -> satTF (And ((Neg x), (Neg y))) bulist (not(va))
        | Imply (x, y) -> satTF (And (x, (Neg y))) bulist (not(va))
        | Iff (x, y) -> satTF (And ((Imply (x, y)), (Imply (x, y)))) bulist va
    in
    (*Var에 TF대입*)
    let rec checkTF f bultable =
        match bultable with
        | [] -> false
        |_ -> if (satTF f bultable true) then true else (satTF f (table bultable) true)
    in
    let start f boolLists = 
        match boolLists with
        | [] -> satTF f boolLists true
        |_ -> checkTF f boolLists
    in  
    start f (makeList f [])
;;

(* problem 4*)
type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

let rec diff : aexp * string -> aexp
= fun (e, x) -> (* TODO *)
    match e with
    | Const a -> Const 0
    | Var a-> if (a=x) then Const 1 else Const 0
    | Power (a, b) -> 
        if (a=x) then Times [Const b ; Power (a, b-1)]
        else Const 0  
    | Times a -> (
        match a with
        [] -> Const 0
        | hd::tl -> (
            Sum [ Times([diff (hd, x)] @ tl) ; Times [hd ; diff (Times tl, x)]]
            )
        )
    | Sum  a -> (
        match a with
        [] -> Const 0
        | hd::tl -> Sum [diff(hd, x) ; diff ((Sum tl), x)]
        )
;;

(* problem 5*)
type exp = X
         | INT of int
         | ADD of exp * exp
         | SUB of exp * exp
         | MUL of exp * exp
         | DIV of exp * exp
         | SIGMA of exp * exp * exp

let rec calculator : exp -> int
= fun e ->
    match e with
        X -> raise(Failure "wrong")
        | INT x -> x
        | ADD (e1, e2) -> 
            let v1 = calculator e1 in
            let v2 = calculator e2 in
              (v1+v2)
        | SUB (e1, e2) -> 
            let v1 = calculator e1 in
            let v2 = calculator e2 in
              (v1-v2)
        | MUL (e1, e2) -> 
            let v1 = calculator e1 in
            let v2 = calculator e2 in
              (v1*v2)
        | DIV (e1, e2) -> 
            let v1 = calculator e1 in
            let v2 = calculator e2 in
              (v1/v2)
        | SIGMA (e1, e2, exp) -> 
            let v1 = calculator e1 in
            let v2 = calculator e2 in
            if v1 = v2 then (
                let rec asdf = fun a b -> (
                match a with
                    X -> asdf b b
                    | INT x -> x
                    | ADD (x1, x2) -> 
                        let y1 = asdf x1 b in
                        let y2 = asdf x2 b in
                          (y1+y2)
                    | SUB (x1, x2) -> 
                        let y1 = asdf x1 b in
                        let y2 = asdf x2 b in
                          (y1-y2)
                    | MUL (x1, x2) -> 
                        let y1 = asdf x1 b in
                        let y2 = asdf x2 b in
                          (y1*y2)
                    | DIV (x1, x2) -> 
                        let y1 = asdf x1 b in
                        let y2 = asdf x2 b in
                          (y1/y2)
                    | SIGMA (x1, x2, expr) -> calculator a
                )
                in asdf exp e2
            )
            else calculator (SIGMA (ADD(e1, INT 1), e2, exp)) + calculator (SIGMA(e1, e1, exp))
;;            
(* problem 6*)
type mobile = branch * branch     (* left and rigth branches *)
and branch = SimpleBranch of length * weight
           | CompoundBranch of length * mobile
and length = int
and weight = int

let rec force br = 
    match br with 
        SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> 
            w1+w2
        | SimpleBranch (l1,w1), CompoundBranch (l2,w2) -> 
            w1+l2*(force w2)
        | CompoundBranch (l1,w1), SimpleBranch (l2,w2) -> 
            (force w1)+w2
        | CompoundBranch (l1,w1), CompoundBranch (l2,w2) -> 
            (force w1)+(force w2)
;;
let rec balanced : mobile -> bool
= fun m -> 
    match m with 
        SimpleBranch (l1,w1), SimpleBranch (l2,w2) -> 
            if (l1 * w1 = l2 * w2) then true else false
        | SimpleBranch (l1,w1), CompoundBranch (l2,w2) -> 
            if (balanced w2) && ((l1 * w1) = (l2 * (force w2))) then true else false
        | CompoundBranch (l1,w1), SimpleBranch (l2,w2) -> 
            if (balanced w1) && (l1*(force w1) = l2*w2) then true else false
        | CompoundBranch (l1,w1), CompoundBranch (l2,w2) -> 
            if (balanced w1)&&(balanced w2)&&(l1*(force w1) = l2*(force w2)) then true else false
;;

(* problem 7*)
type digit = ZERO | ONE
type bin = digit list

let rec cnt : bin -> int
= fun l -> 
    match l with
    [] -> 0
    | hd::tl -> 1+ cnt tl
;;

let rec fastexpt : int -> int -> int
= fun b n -> if n = 0 then 1
else if (n mod 2 = 0) then (fastexpt b (n/2))*(fastexpt b (n/2))
else b*(fastexpt b (n-1))
;;

let rec biToDe bi =
    match bi with
    [] -> 0
    | hd::tl->
        match hd with
        | ZERO -> biToDe tl
        | ONE -> (fastexpt 2 ((cnt bi)-1)) + (biToDe tl)
;;

let rec deToBi de = 
    if de = 0 then []
    else  if ((de mod 2)=1) then (deToBi ((de-1)/2))@[ONE]
    else (deToBi (de/2))@[ZERO]
;;

let bmul : bin -> bin -> bin
= fun b1 b2 -> 
    deToBi ((biToDe b1)*(biToDe b2))
;;