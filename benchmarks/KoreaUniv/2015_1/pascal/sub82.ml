(*first homework*)


(* Problem 1 *)

let rec pascal : int * int -> int
=fun (x,y) -> match (x,y) with   
  |(0,0) -> 1
  |(-1,_) -> 0
  |(_,-1) ->0
  |_ -> pascal(x-1,y-1)+pascal(x-1,y) ;;

