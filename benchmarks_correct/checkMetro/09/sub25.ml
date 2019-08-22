(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 8
 *)

type lambda = V of var
	     | P of var * lambda
	     | C of lambda * lambda
and var = string;;

let check m = 
  let rec myCheck m l =
    match m with
	V(var) -> 
	  List.mem var l
      |	P(var,lambda) ->
	  myCheck lambda (var::l)
      | C(lambda1,lambda2) ->
	  (myCheck lambda1 l)&(myCheck lambda2 l)
  in
    myCheck m [];;
