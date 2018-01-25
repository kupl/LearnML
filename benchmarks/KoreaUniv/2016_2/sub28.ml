(*********************)
(*     Problem 1     *)
(*********************)
let rec max : int list -> int
= fun lst ->
match lst with
|hd::tl -> if tl = [] then hd
              else if hd>max(tl) then hd
              else max(tl)
|[]-> raise (Failure "No element in list")


let rec min : int list -> int
= fun lst ->
match lst with
|hd::tl -> if tl = [] then hd
              else if hd<min(tl) then hd
              else min(tl)
|[]-> raise (Failure "No element in list")



(*********************)
(*     Problem 2     *)
(*********************)
let rec filter pred lst =
match lst with
|[]->[]
|hd::tl -> if pred(hd) then hd::(filter pred tl)
else filter pred tl


(*********************)
(*     Problem 3     *)
(*********************)
let rec double f a =
f(f(a))

(*********************)
(*     Problem 4     *)
(*********************)
type btree =
        | Empty
        | Node of int * btree * btree

let rec mem : int -> btree -> bool
= fun n tree ->
match tree with
|Empty -> false
|Node(a, b, c) ->
    if n=a then true
     else if mem n c  then true
    else if mem n b then true
        else false


(*********************)
(*     Problem 5     *)
(*********************)
type nat =
        | ZERO
        | SUCC of nat

let rec natadd : nat -> nat -> nat
= fun a b ->
let rec count nat =
match nat with
| ZERO ->0
|SUCC(a) -> 1 + count a
in
let rec add num nat =
if num =0 then nat
else add (num-1) (SUCC(nat))
in
let cnt = count(a) in
add cnt b


let rec natmul : nat -> nat -> nat
= fun a b ->
let rec count nat =
match nat with
| ZERO ->0
|SUCC(a) -> 1 + count a
in
let rec add num nat =
if num =0 then nat
else add (num-1) (SUCC(nat))
in
let ca = count(a) in
let cb = count(b) in
add (ca*cb) (ZERO)


(*********************)
(*     Problem 6     *)
(*********************)
type formula =
        | True
        | False
        | Not of formula
        | AndAlso of formula * formula
        | OrElse of formula * formula
        | Imply of formula * formula
        | Equal of exp * exp

and exp =
        | Num of int
        | Plus of exp * exp
        | Minus of exp * exp
let rec cnt ex =
match ex with
        |Num(a) ->a
        |Plus(a,b)->cnt(a)+cnt(b)
        |Minus(a,b) -> cnt(a)-cnt(b)


let rec eval : formula -> bool
= fun f ->
match f with
|True -> true
|False -> false
|Not(formula) ->
        if eval(formula)=true then false
        else true
|AndAlso(a, b) ->
        if eval(a)=true && eval(b)= true then true
        else false
|OrElse(a,b) ->
        if eval(a) =true || eval(b) = true then true
        else false
|Imply(a,b) ->
        if eval(a)=false || eval(b) = true then true
        else false
|Equal(a,b)->
        if cnt(a)=cnt(b) then true
        else false