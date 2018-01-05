type crazy2 =
NIL
|ONE of crazy2
|ZERO of crazy2
|MONE of crazy2;;

exception WrongInput;;

let rec crazy2add (crz1,crz2) =
 let rec foo c1 c2 cr =
  match c1,c2,cr with
  |NIL,_,0 -> c2
  |_,NIL,0 -> c1
  |NIL,_,1 -> let x = ONE NIL in (foo c2 x 0)
  |_,NIL,1 -> let x = ONE NIL in (foo c1 x 0)
  |NIL,_,-1 -> let x = MONE NIL in (foo c2 x 0)
  |_,NIL,-1 -> let x = MONE NIL in (foo c1 x 0)
  |ONE(a),ONE(b),1 -> ONE (foo a b 1)
  |ONE(a),ONE(b),0 -> ZERO (foo a b 1)
  |ONE(a),ONE(b),-1 -> ONE (foo a b 0)
  |ONE(a),ZERO(b),1 -> ZERO (foo a b 1)
  |ONE(a),ZERO(b),0 -> ONE (foo a b 0)
  |ONE(a),ZERO(b),-1 -> ZERO (foo a b 0)
  |ONE(a),MONE(b),1 -> ONE (foo a b 0)
  |ONE(a),MONE(b),0 -> ZERO (foo a b 0)
  |ONE(a),MONE(b),-1 -> MONE (foo a b 0)
  |ZERO(a),ONE(b),1 -> ZERO (foo a b 1)
  |ZERO(a),ONE(b),0 -> ONE (foo a b 0)
  |ZERO(a),ONE(b),-1 -> ZERO (foo a b 0)
  |ZERO(a),ZERO(b),1 -> ONE (foo a b 0)
  |ZERO(a),ZERO(b),0 -> ZERO (foo a b 0)
  |ZERO(a),ZERO(b),-1 -> MONE (foo a b 0)
  |ZERO(a),MONE(b),1 -> ZERO (foo a b 0)
  |ZERO(a),MONE(b),0 -> MONE (foo a b 0)
  |ZERO(a),MONE(b),-1 -> ZERO (foo a b -1)
  |MONE(a),ONE(b),1 -> ONE (foo a b 0)
  |MONE(a),ONE(b),0 -> ZERO (foo a b 0)
  |MONE(a),ONE(b),-1 -> MONE (foo a b 0)
  |MONE(a),ZERO(b),1 -> ZERO (foo a b 0)
  |MONE(a),ZERO(b),0 -> MONE (foo a b 0)
  |MONE(a),ZERO(b),-1 -> ZERO (foo a b -1)
  |MONE(a),MONE(b),1 -> MONE (foo a b 0)
  |MONE(a),MONE(b),0 -> ZERO (foo a b -1)
  |MONE(a),MONE(b),-1 -> MONE (foo a b -1)
  |_ -> raise WrongInput
 in
 (foo crz1 crz2 0)
;;
