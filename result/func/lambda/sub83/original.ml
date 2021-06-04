type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec input_var (e : lambda) (lst : string list) : string list =
  match e with
  | V v -> lst
  | P (v, e1) -> input_var e1 lst @ [ v ]
  | C (e1, e2) -> input_var e1 (input_var e2 lst)


let rec input_lambda (e : lambda) (lst : string list) : string list =
  match e with
  | V v -> lst @ [ v ]
  | P (v, e1) -> input_lambda e1 lst
  | C (e1, e2) -> input_lambda e1 (input_lambda e2 lst)


let rec isthere (lst : string list) (l2 : string list) : bool =
  match lst with
  | [] -> true
  | hd :: tl -> (
      match l2 with
      | [] -> false
      | h :: t -> if hd = h then true else isthere [ hd ] t )


let rec check (lambda : lambda) : bool =
  let l2 : string list = input_var lambda [] in

  let l1 : string list = input_lambda lambda [] in

  match lambda with
  | V v -> false
  | C (e1, e2) -> check e1 && check e2
  | P (v, e) -> (
      match l1 with
      | [] -> true
      | hd :: tl -> if isthere [ hd ] l2 then isthere tl l2 else false )
