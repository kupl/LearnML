(*
 * Brief      : HW2, Program Language (4190.310)
 * Author     : YongKi Kim <kim.yongki@ropas.snu.ac.kr>
 * Student Id : 2014-21767
 * Date       : Sep. 23, 2014
 *)

(* Exercise 2 : Leftist Heap *)

type heap = EMPTY | NODE of rank * value * heap * heap
and rank = int
and value = int

exception EmptyHeap
let rank = function EMPTY -> 0
                  | NODE(r,_,_,_) -> r

let shake = function (x,lh,rh) ->
  if (rank lh) >= (rank rh)
  then NODE(rank rh + 1, x, lh, rh)
  else NODE(rank lh + 1, x, rh, lh)

let print_heap h =
  let rec print_heap_sub h l =
    let print_space w = print_string (String.make w '.') in
    match h with
    | EMPTY -> print_string ""
    | NODE(r,x,lh,rh) -> print_string "\n"; print_space l; Printf.printf "(%d:%d)\n" x r;
     	if lh != EMPTY then (print_space l; print_string "l->"; print_heap_sub lh (l+1));
     	if rh != EMPTY then (print_space l; print_string "r->"; print_heap_sub rh (l+1))
  in print_heap_sub h (0)

(* todo *)
let rec merge : heap * heap -> heap = fun (h1,h2) ->
  let h1,h2 = match h1,h2 with
  | NODE(_,v1,_,_),NODE(_,v2,_,_) -> if v1 > v2 then (h2,h1) else (h1,h2)
  | EMPTY,_ |_,EMPTY -> (h1,h2) in
  match (h1,h2) with
  | EMPTY, h | h, EMPTY -> h
  | NODE(r1,x1,lh1,rh1),NODE(_,_,_,_) ->
    match (if rh1 = EMPTY then NODE(r1,x1,lh1,h2) else NODE(r1,x1,lh1, merge(rh1,h2))) with
    | NODE(_,x,lh,rh) -> shake(x,lh,rh)
    | EMPTY -> EMPTY

let insert = function (x,h)  -> merge(h, NODE(0,x,EMPTY,EMPTY))
let findMin = function EMPTY -> raise EmptyHeap
                     | NODE(_,x,_,_) -> x
let deleteMin = function EMPTY -> raise EmptyHeap
| NODE(_,_,lh,rh) -> merge(lh,rh)