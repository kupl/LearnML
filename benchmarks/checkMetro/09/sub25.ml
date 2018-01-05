(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 8
 *)

type metro = STATION of name
	     | AREA of name * metro
	     | CONNECT of metro * metro
and name = string;;

let checkMetro m = 
  let rec myCheck m l =
    match m with
	STATION(name) -> 
	  List.mem name l
      |	AREA(name,metro) ->
	  myCheck metro (name::l)
      | CONNECT(metro1,metro2) ->
	  (myCheck metro1 l)&(myCheck metro2 l)
  in
    myCheck m [];;
