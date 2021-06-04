type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let insert a (l : 'a list) : 'a list =
  match l with [] -> [ a ] | hd :: tl -> a :: hd :: tl


let rec confirm a (l : 'b list) : bool =
  match l with [] -> false | hd :: tl -> if hd = a then true else confirm a tl


let rec cal ((a : string list), (b : lambda)) : string list =
  match b with
  | V v -> if confirm v a then a else insert "false" a
  | P (x, y) -> if confirm x a then cal (a, y) else cal (insert x a, y)
  | C (x, y) -> cal (cal (a, x), y)


let check (e : lambda) : bool =
  match e with
  | V x -> false
  | P (x, y) ->
      let l : string list = cal (insert x [], y) in
      if confirm "false" l then false else true
  | C (x, y) -> false
