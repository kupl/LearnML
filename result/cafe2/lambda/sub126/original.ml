type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec test (lam : lambda) : (string * string) list =
  match lam with
  | V x -> [ ("v", x) ]
  | P (x, l) ->
      let lst : (string * string) list = test l in
      [ ("p", x) ] @ lst
  | C (l1, l2) ->
      let lst1 : (string * string) list = test l1 in

      let lst2 : (string * string) list = test l2 in
      lst1 @ lst2


let rec find num (lst : 'a list) : 'a list =
  match lst with
  | [] -> []
  | hd :: tl -> if hd = num then find num tl else [ hd ] @ find num tl


let rec remove (lst : (string * 'b) list) : (string * 'b) list =
  match lst with
  | [] -> []
  | hd :: tl -> (
      match hd with
      | p, x ->
          if p = "p" then
            let lst1 : (string * 'b) list = find ("v", x) tl in
            remove lst1
          else if p = "v" then hd :: remove tl
          else raise Failure "a" )


let rec order (lst : (string * 'c) list) : (string * 'c) list =
  match lst with
  | [] -> []
  | hd :: tl -> (
      match hd with p, x -> if p = "p" then order tl else hd :: order tl )


let rec check (lam : lambda) : bool =
  let lst1 : (string * string) list = test lam in

  let lst2 : (string * string) list = remove lst1 in

  let lst3 : (string * string) list = order lst2 in

  match lst3 with [] -> true | _ -> false
