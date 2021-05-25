   type lambda = V of var
           | P of var * lambda
           | C of lambda * lambda
   and var = string
  
   let rec search x e =
   match e with
      []->false
      |  h::t -> if h=x then true else search x t


   let rec sub_check : lambda -> string list -> bool 
   = fun e l ->
   match e with
      V v -> search v l
      |P(v,e1)->sub_check e1 (v::l)
      |C(e1,e2)->sub_check e1 l && sub_check e2 l

   let check : lambda -> bool
   =fun e ->
   sub_check e []
