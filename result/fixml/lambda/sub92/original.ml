type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec bound : lambda -> var list =
 fun lambda ->
  match lambda with
  | V var -> []
  | P (var, ex) -> var :: bound ex
  | C (ex1, ex2) -> bound ex1 @ bound ex2


let rec variables : lambda -> var list =
 fun lambda ->
  match lambda with
  | V var -> [ var ]
  | P (var, ex) -> variables ex
  | C (ex1, ex2) -> variables ex1 @ variables ex2


let rec containHelper : var list * var -> bool =
 fun (bound, a) ->
  match (bound, a) with
  | [], a -> false
  | x :: tl, a -> if x = a then true else containHelper (tl, a)


let rec contain : var list * var list -> bool =
 fun (bound, variables) ->
  match (bound, variables) with
  | [], variables -> false
  | bound, [] -> true
  | bound, a :: tl ->
      if containHelper (bound, a) = false then false else contain (bound, tl)


let check : lambda -> bool =
 fun lambda -> contain (bound lambda, variables lambda)
