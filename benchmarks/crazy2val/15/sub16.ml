(* 2010-11753 snucse Taekmin Kim *)
(* HW 2-1 *)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec pow = fun(n, k) ->
  if k == 0 then 1
  else n * pow(n, k - 1)

let rec crazy2valWithK: crazy2 * int -> int = fun(c ,k) ->
  match c with
  | NIL -> 0
  | ZERO c2 -> crazy2valWithK(c2, k + 1)
  | ONE c2 -> crazy2valWithK(c2, k + 1) + pow(2, k)
  | MONE c2 -> crazy2valWithK(c2, k + 1) - pow(2, k)

let rec crazy2val: crazy2 -> int = fun(c) ->
  crazy2valWithK(c, 0)


(*let _ = print_endline(string_of_int(crazy2val(ZERO(ONE(MONE NIL)))))) *)

(*
let _= 
  let print_bool x = print_endline (string_of_bool x) in 
  print_bool (-1  = (crazy2val (MONE NIL))); 
  print_bool (1   = (crazy2val (ONE (ZERO (ZERO (ZERO NIL)))))); 
  print_bool (1   = (crazy2val (ONE NIL))); 
  print_bool (9   = (crazy2val (MONE (MONE (ONE (ONE (ZERO NIL))))))); 
  print_bool (-13 = (crazy2val (MONE (ZERO (ONE (ZERO (ONE (MONE NIL)))))))) 
;; 
*)
