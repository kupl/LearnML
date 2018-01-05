(* ID : 2007-12138 *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap

let rank = function EMPTY -> -1
                  | NODE(r,_,_,_) -> r;;

let findMin = function EMPTY -> raise EmptyHeap
                     | NODE(_,x,_,_) -> x;;

let shake = function (x,lh,rh) ->
  if (rank lh) >= (rank rh)
    then NODE(rank rh + 1, x, lh, rh)
    else NODE(rank lh + 1, x, rh, lh);;

(*Personal Functions - defined for convenience
nval : takes out the value of node
get_h1 : gets the left decendant of target
get_h2 : gets the right decendant of target *)

let nval = function EMPTY -> 0
                  | NODE(_,x,_,_) -> x;;
let get_h1 h =
  match h with
   EMPTY -> EMPTY
  |NODE(_,_,h1,_) -> h1;;

let get_h2 h =
  match h with
   EMPTY -> EMPTY
  |NODE(_,_,_,h2) -> h2;;
(*Personal Functions end*)

let rec merge(h1,h2) = 
  match (h1,h2) with
     (EMPTY,EMPTY) -> EMPTY  (* Base cases are obvious *)
   | (h1,EMPTY) -> h1
   | (EMPTY,h2) -> h2
    (* Inductive step : let the node with smaller value left and upmost *)
   | (h1,h2) -> if (nval h1) <= (nval h2)                              
		  (* When the value of h1 <= h2, put left tree of h1 left, and put the result of merge (right tree of h1, h2
                     => Then, the top node is the upmost node of h1 *)  
		  then shake(nval(h1), get_h1(h1), merge(get_h2(h1), h2))
                  (*Else,  put left tree of h2 left, and put the result of merge (right tree of h2, h1) 
                     => Then, the top node is the upmost node of h2 *)
		  else shake(nval(h2), get_h1(h2), merge(get_h2(h2), h1));;

let insert = function (x,h) -> merge(h, NODE(0,x,EMPTY,EMPTY));;

let deleteMin = function EMPTY -> raise EmptyHeap
                       | NODE(_,x,lh,rh) -> merge(lh,rh);;


