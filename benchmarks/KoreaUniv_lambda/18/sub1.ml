type lambda = 
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec helper : lambda -> 'a list -> bool
= fun l s ->
  match l with
    | V (x) ->
      (try (fun _ -> true) @@ List.find (fun y -> if x = y then true else false) s
      with _ -> false)
  	| P (x,y) -> helper y (x::s)
  	| C (x1,x2) -> helper x1 s && helper x2 s

let check : lambda -> bool
= fun lam -> helper lam [];;

