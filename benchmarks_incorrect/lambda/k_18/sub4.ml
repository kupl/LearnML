type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec lookup : var list -> var -> bool
= fun board str -> match board with
                    | hd::tl -> if hd = str then true else lookup tl str
                    | [] -> false;;

(*let rec find_element : var list -> var -> var list
= fun board str -> match board with
                    | hd::tl -> if hd = str then tl else hd :: (find_element tl str)
                    | [] -> board;;*)

                            
let rec scoring : var list -> var list -> lambda -> (var list * var list)
= fun p v lam -> match lam with
                  | P (x, lams) -> if (lookup p x) then scoring p v lams
                                   else let new_P = x :: p 
                                        in scoring new_P v lams
                  | C (lam1, lam2) -> let (n_P, n_V) = scoring p v lam1
                                      in let (new_P, new_V) = scoring n_P n_V lam2
                                         in (new_P, new_V)
                  | V (x) -> let new_V = x :: v
                             in (p, new_V);;
(*                             
let rec scoringP : var list -> lambda -> var list
= fun p lam -> match lam with
                  | P (x, lams) -> if (lookup p x) then scoringP p lams
                                   else let new_P = x :: p 
                                        in scoringP new_P lams
                  | C (lam1, lam2) -> let n_P = scoringP p lam1
                                      in let new_P = scoringP n_P lam2
                                         in new_P
                  | V (x) -> p;;
                  
let rec scoringV : var list -> lambda -> var list
= fun v lam -> match lam with
                  | P (x, lams) -> scoringV v lams
                  | C (lam1, lam2) -> let n_V = scoringV v lam1
                                      in let new_V = scoringV n_V lam2
                                         in new_V
                  | V (x) -> let new_V = x :: v
                             in new_V;;                  
                            *)
let rec checking : var list -> var list -> bool
= fun p v -> match v with
              | hd::tl -> if (lookup p hd) then checking p tl else false
              | [] -> true;;
              
                    
let check : lambda -> bool
= fun lam -> let boardP = []
             in let boardV = []
                in let (resultP, resultV) = scoring boardP boardV lam
                   in checking resultP resultV;;
                    

                
check (P ("a", V "a"));;
check (P ("a", P ("a", V "a")));;
check (P ("a", P ("b", C (V "a", V "b"))));;
check (P ("a", C (V "a", P ("b", V "a"))));;

check (P ("a", V "b"));;
check (P ("a", C (V "a", P ("b", V "c"))));;
check (P ("a", P ("b", C (V "a", V "c"))));;


