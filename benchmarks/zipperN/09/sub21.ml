(* C:\Documents and Settings\Administrator\¹ÙÅÁ È­¸é\pl_first_homework\Exercise4.ml *)

(* Computer S&E/2007-15612/park sungjun*)
(*Exercise 4*)
let rec zipperN listN =
  	let rec f1 lista= (* at list*)
  	match lista with
  	|[] -> []
  	|h :: t -> [h]  in          
  	
	let rec f2 listb=
  	match listb with (*at list of list*)
  	|[] -> []
  	|h :: t ->(f1 h)@f2(t) in
  
  	let rec f3 listc=
  	match listc with
  	|[] -> []
  	|h :: t -> [t]  in 
  	
  	let rec f4 listd=
  	match listd with
  	|[] ->[]
  	|h :: t -> (f3 h)@f4(t)  in  
   
	match listN with
  	|[] -> []
  	|h :: t -> (f2 listN)@zipperN(f4 listN )
  ;;

