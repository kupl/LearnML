  type exp = V of var
           | P of var * exp
           | C of exp * exp
  and var = string
  


let check a =

   let rec isinlst l x =
   match l with
   [] -> false
   |hd::tl -> if (hd = x) then true else isinlst tl x in

   let rec letsdoit e lst =
   match e with
   V i -> isinlst lst i
   |P(i,j) -> letsdoit j (i :: lst)
   |C(i,j) -> (letsdoit i lst)&&(letsdoit j lst)
 in letsdoit a [];;
