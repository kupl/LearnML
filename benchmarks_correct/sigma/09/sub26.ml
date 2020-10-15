
(*
 * Student no. : 2009-20769
 * Name        : Kim, Seongjun
 *)

exception Error of string

let sigma f a b =
  let rec s a b f =
    if a > b then 0
    else (f a) + (s (a+1) b f)
  in

    if a > b then raise (Error "Initial value is grater than final value")
    else s a b f

(*
;;
sigma (1, 0, (fun n -> n));;     (* Exception *)
sigma (1, 10, (fun n -> n));;    (* 55 *)
sigma (1, 4, (fun n-> n*n));;    (* 30 *)
sigma (1, 1, (fun n -> n));;     (* 1 *)
*)
