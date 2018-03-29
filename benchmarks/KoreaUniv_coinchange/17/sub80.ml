(* problem 8*)


  let rec remove a l =
  match l with
  | [] -> []
  | hd::tl -> if hd = a then tl else remove a tl;;
  
  let n = 0;;

  let change : int list -> int -> int
= fun coins amount -> 
  match coins with
  | [] -> 0 
  | hd::tl -> if amount mod hd <> 0 then fun (remove hd coins) (amount-hd*amount mod hd) 
  else n = n+1 in while(amount / hd <>0 ) do fun (remove hd coins) (amount-hd*amount mod hd) done
  n;; 

  