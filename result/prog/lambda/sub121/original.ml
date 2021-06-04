type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

type bounded = var list

let update_bounded (a : string) (stk : 'a list) : 'a list = a :: stk

let rec find_bounded a (stk : 'b list) : bool =
  match stk with
  | [] -> false
  | hd :: tl -> if hd = a then true else find_bounded a tl


let rec boundCheck (lam : lambda) (bd : string list) : bool =
  match lam with
  | V v -> find_bounded v bd
  | P (v, l) ->
      let s : string list = update_bounded v bd in
      boundCheck l s
  | C (l1, l2) -> if boundCheck l2 bd then boundCheck l2 bd else false


let rec check (lam : lambda) : bool = boundCheck lam []
