let idf = fun x-> x;;
let compose f g = fun x -> f(g(x));;

let rec iii f n 
= if n=0 then idf
  else compose f (iii f (n-1)) ;;

let iter : int * (int -> int) -> (int -> int)
= fun (n,f) -> 

 iii f n ;;



iter (3, fun x->2+x) 0;;  

