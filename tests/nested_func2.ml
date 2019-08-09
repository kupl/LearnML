let add16 : int -> int
= fun x -> 
  let var1 = 150 in
  let add8 : int -> int
  = fun y ->
    let add4: int -> int
    = fun z -> 
      let add2: int -> int
      = fun a -> a+2
    in add2 (add2 z)
  in add4 (add4 y)
in add8 (add8 x)

(*
 * add16 -> let var1 = 150 in
 *          add8 (add8 x)
 * add8 -> add4 (add4 y)
 * add4 -> add2 (add2 z)
 * add2 -> a+2
 *           
 *
 *
 *
 *
 *
 *)
