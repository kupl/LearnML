(* 2010-11753 snucse Taekmin Kim *)
(* HW 2-2 *)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2

let rec crazy2add: crazy2 * crazy2 -> crazy2 = fun(cx, cy) ->
  match cx, cy with
  | NIL, _ -> cy
  | _, NIL -> cx

  | ZERO cx1, ZERO cx2 -> (ZERO (crazy2add(cx1, cx2)))
  | ONE cx1, MONE cx2 -> (ZERO (crazy2add(cx1, cx2)))
  | MONE cx1, ONE cx2 -> (ZERO (crazy2add(cx1, cx2)))

  | ZERO cx1, ONE cx2 -> (ONE (crazy2add(cx1, cx2)))
  | ONE cx1, ZERO cx2 -> (ONE (crazy2add(cx1, cx2)))

  | ZERO cx1, MONE cx2 -> (MONE (crazy2add(cx1, cx2)))
  | MONE cx1, ZERO cx2 -> (MONE (crazy2add(cx1, cx2)))

  | MONE cx1, MONE cx2 -> (ZERO (crazy2add((MONE NIL), crazy2add(cx1, cx2))))
  | ONE cx1, ONE cx2 -> (ZERO (crazy2add((ONE NIL), crazy2add(cx1, cx2))))


  (*
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
;; 

let a = ONE (ONE NIL) 
let b = ONE (ONE (ZERO NIL)) 

let _ = print_int(crazy2val (crazy2add(a,b)) )
let _ = print_int(crazy2val(a) + crazy2val(b) )

let _= 
  let print_bool x = print_endline (string_of_bool x) in 

  print_bool (0 = (crazy2val (crazy2add (ZERO NIL, ZERO NIL)))); 
  print_bool (0 = (crazy2val (crazy2add (MONE NIL, ONE NIL)))); 
  print_bool (1 = (crazy2val (crazy2add (ZERO NIL, ONE NIL)))); 
  print_bool (4 = (crazy2val (crazy2add (ONE (ONE NIL), ONE NIL)))); 
  print_bool (-683 = (crazy2val (crazy2add (MONE (ZERO (ZERO (ZERO NIL))), (ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL))))))))))))))) 
;; 
*)
