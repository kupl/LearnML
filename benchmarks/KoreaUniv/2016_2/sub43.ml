let rec max lst = 
match lst with
| [] -> 0
| x::[] -> x
| x::xs ->
     let v = max xs in
       if x < v then
            v
       else
           x



let rec min lst =
match lst with
| [] -> 0
| x::[] -> x
| x::xs ->
     let v = min xs in
       if x < v then 
            x
       else
          v 

let rec fold_right (f: 'a -> 'b -> 'b) (lst : 'a list) (acc : 'b) : 'b =
  match lst with
     [] -> acc
     | x :: xs -> f x (List.fold_right f xs acc)




let filter f lst =
  List.fold_right (fun x a -> if f x then x :: a else a) lst []

let double (f : 'a -> 'a ) (x : 'a) : 'a  = f (f x)




type btree=
Empty
|Node of int * btree * btree

let rec mem x btree = 
   match btree with
   Empty -> false
    | Node(y,left,right) ->
      if x = y then true else
      if x < y then mem x left else mem x right



type nat = ZERO | SUCC of nat


let rec natadd (a : nat) (b : nat) : nat =
   match a with
      ZERO -> b
    | SUCC c -> natadd c (SUCC b)


let rec natmul (a : nat) (b :nat) : nat =
   match a with
     ZERO -> ZERO
   | SUCC c -> natadd b (natmul c b)



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



let rec eval formula a = 
  match formula with
     True -> true
    |False -> false
    |Not(p) -> not(eval p a)
    |AndAlso(p,q) -> (eval p a) && (eval q a)
    |OrElse(p,q) -> (eval p a) || (eval q a)
    |Imply(p,q) -> not(eval p a) || (eval q a)
    |Equal(p,q) -> true
