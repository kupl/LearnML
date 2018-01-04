(* School of Computer Science & Engineering
 * 2009-23151
 * 조성근
 * HW 1 - Exercise 3
 *)

let rec zipper ((a:int list),(b:int list)) = match a with
    [] -> b
  | ahead::atail -> 
      match b with
	  [] -> (ahead::atail)
	| bhead::btail -> ahead::bhead::(zipper (atail,btail));;
