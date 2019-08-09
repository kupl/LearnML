let add16 : int -> int
= fun x -> 
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
 * add16 = add8 (add8 x)
 * add8 = add4 (add4 y)
 * add4 = add2 (add2 z)
 * add2 = a+2 
 * DLet 
 * --BindOne "add16" 
 * --Type (int -> int) = 
 * --EFun ArgOne Tvar "x" -> ELet 
 * ------BindOne "add8" 
 * ------Type (int -> int) = 
 * ------EFun ArgOne Tvar "y" -> ELet 
 * ----------BindOne "add4" 
 * ----------Type (int -> int) = 
 * ----------EFun ArgOne Tvar "z" -> ELet 
 * --------------BindOne "add2"  
 * --------------Type (int -> int) = 
 * --------------EFun ArgOne Tvar "a" -> ADD ("a" + Const 2) 
 *           inEApp ("add2" (EApp ("add2" ("z")))) 
 *       inEApp ("add4" (EApp ("add4" ("y")))) 
 *    inEApp ("add8" (EApp ("add8" ("x"))))
 *)

