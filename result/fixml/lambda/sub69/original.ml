type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let insert a l = match l with [] -> [ a ] | hd :: tl -> a :: hd :: tl

let rec confirm a l =
  match l with [] -> false | hd :: tl -> if hd = a then true else confirm a tl


let rec cal : string list * lambda -> string list =
 fun (a, b) ->
  match b with
  | V v -> if confirm v a then a else insert "false" a
  | P (x, y) -> if confirm x a then cal (a, y) else cal (insert x a, y)
  | C (x, y) -> cal (cal (a, x), y)


let check : lambda -> bool =
 fun e ->
  match e with
  | V x -> false
  | P (x, y) ->
      let l = cal (insert x [], y) in
      if confirm "false" l then false else true
  | C (x, y) -> false
