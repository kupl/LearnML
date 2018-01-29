
 (* problem 4*)
 type aexp =
  | Const of int
  | Var of string
  | Power of string * int
  | Times of aexp list
  | Sum of aexp list

 let rec diff : aexp * string -> aexp
 = fun (e,x) ->
 match (e,x) with
 | Power (e, x) ->( match x with
   |0 -> 1
   |_ -> e*(Power (e,x-1)))
 | Times lst -> (match lst with
   | []->1
   | hd::tl -> hd*(Times tl))
 | Sum lst ->
  ( match lst with
  |[] -> 0
  | h::t -> h+(Sum t))*)
