type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec str (lambda : lambda) : string list =
  match lambda with
  | V a -> []
  | P (a, e1) -> a :: str e1
  | C (e1, e2) -> str e1 @ str e2


let rec v_str (lambda : lambda) : string list =
  match lambda with
  | V a -> [ a ]
  | P (a, e1) -> List.filter (fun (__s11 : string) -> __s11 != a) (v_str e1)
  | C (e1, e2) -> v_str e1 @ v_str e2


let rec search var (lambda : 'a list) : bool =
  match lambda with
  | [] -> false
  | hd :: tl -> if hd = var then true else search var tl


let rec compare (e1 : 'b list) (e2 : 'b list) : bool =
  match e2 with
  | [] -> true
  | hd :: tl -> if search hd e1 && compare e1 tl then true else false


let check (lambda : lambda) : bool = List.length (v_str lambda) = 0
