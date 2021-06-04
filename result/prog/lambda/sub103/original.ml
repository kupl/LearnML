type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let listP : string list = [ "" ]

let rec check (lambda : lambda) : bool =
  match lambda with
  | V x -> List.mem x listP
  | P (x, e1) ->
      let listP : string list = listP @ [ x ] in
      check e1
  | C (e1, e2) -> check e1 && check e2
