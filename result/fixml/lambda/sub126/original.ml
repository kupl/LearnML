type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec test lam =
  match lam with
  | V x -> [ ("v", x) ]
  | P (x, l) ->
      let lst = test l in
      [ ("p", x) ] @ lst
  | C (l1, l2) ->
      let lst1 = test l1 in

      let lst2 = test l2 in
      lst1 @ lst2


let rec find num lst =
  match lst with
  | [] -> []
  | hd :: tl -> if hd = num then find num tl else [ hd ] @ find num tl


let rec remove lst =
  match lst with
  | [] -> []
  | hd :: tl -> (
      match hd with
      | p, x ->
          if p = "p" then
            let lst1 = find ("v", x) tl in
            remove lst1
          else if p = "v" then hd :: remove tl
          else raise Failure "a" )


let rec order lst =
  match lst with
  | [] -> []
  | hd :: tl -> (
      match hd with p, x -> if p = "p" then order tl else hd :: order tl )


let rec check : lambda -> bool =
 fun lam ->
  let lst1 = test lam in

  let lst2 = remove lst1 in

  let lst3 = order lst2 in

  match lst3 with [] -> true | _ -> false
