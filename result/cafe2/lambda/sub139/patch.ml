type lambda = V of var | P of (var * lambda) | C of (lambda * lambda)

and var = string

let rec lookup (board : string list) (str : string) : bool =
  match board with
  | hd :: tl -> if hd = str then true else lookup tl str
  | [] -> false


let rec scoring (p : string list) (v : string list) (lam : lambda) :
    var list * var list =
  match lam with
  | P (x, lams) ->
      if lookup p x then scoring p v lams
      else
        let new_P : string list = x :: p in
        scoring new_P v lams
  | C (lam1, lam2) ->
      let (n_P, n_V) : string list * string list = scoring p v lam1 in

      let (new_P, new_V) : string list * string list = scoring n_P n_V lam2 in
      (new_P, new_V)
  | V x ->
      let new_V : string list = x :: v in
      (p, new_V)


let rec checking (p : string list) (v : string list) : bool =
  match v with
  | hd :: tl -> if lookup p hd then checking p tl else false
  | [] -> true


let rec __s3 (__s4 : lambda) : string list =
  match __s4 with
  | V __s5 -> [ __s5 ]
  | P (__s6, __s7) ->
      List.filter (fun (__s8 : string) -> not (__s6 = __s8)) (__s3 __s7)
  | C (__s9, __s10) -> __s3 __s9 @ __s3 __s10


let check (lam : lambda) : bool =
  let boardP : string list = [] in

  let boardV : string list = [] in

  let (resultP, resultV) : string list * string list =
    scoring boardP boardV lam
  in
  List.length (__s3 lam) = 0


let (_ : bool) = check (P ("a", V "a"))

let (_ : bool) = check (P ("a", P ("a", V "a")))

let (_ : bool) = check (P ("a", P ("b", C (V "a", V "b"))))

let (_ : bool) = check (P ("a", C (V "a", P ("b", V "a"))))

let (_ : bool) = check (P ("a", V "b"))

let (_ : bool) = check (P ("a", C (V "a", P ("b", V "c"))))

let (_ : bool) = check (P ("a", P ("b", C (V "a", V "c"))))
