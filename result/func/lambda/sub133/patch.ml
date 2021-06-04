type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let rec check (lam : lambda) : bool =
  let rec get (lam : lambda) (lst1 : string list) (lst2 : string list) :
      string list * string list =
    match lam with
    | V x -> (lst1, x :: lst2)
    | P (x, e) ->
        let lst' : string list = x :: lst1 in
        get e lst' lst2
    | C (e1, e2) ->
        let t : string list * string list = get e1 lst1 lst2 in
        get e2 (fst t) (snd t)
  in

  let rec search (lst1 : string list) (lst2 : string list) : bool =
    match lst2 with
    | [] -> true
    | hd :: tl ->
        let rec find (p : string) (lst : string list) : bool =
          match lst with
          | [] -> false
          | hd :: tl -> if hd = p then true else find p tl
        in
        if find hd lst1 then search lst1 tl else false
  in
  List.length (__s3 lam) = 0
