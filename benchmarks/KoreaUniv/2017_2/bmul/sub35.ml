
(*problem 7*)

  type digit = ZERO | ONE
  type bin = digit list
      
  let rec decimal: bin-> int 
  =fun b->
      let num=0 in
      match b with
      |[]-> num
      |h::t-> 
(*
            match h with
            |ZERO-> decimal t
            |ONE-> 
*)
          if h =ONE 
          then let num = num + int_of_float (2.0**float_of_int (List.length t)) 
               in num
          else decimal t 
        
  let rec findbinary: int-> bin-> bin
  =fun x lst ->
    match x with
    |0->[ZERO]@lst
    |1->[ONE]@lst
    |_-> findbinary (x/2) (if x mod 2 = 0 then [ZERO]@lst else [ONE]@ lst)
    (*
findbinary (x/2) (lst::(if y mod 2 = 0 then ZERO else ONE))
  *)

  let bmul: bin->bin->bin
  =fun b1 b2->
      let a= decimal(b1)*decimal(b2) in
      findbinary a []  

  (*
      let rec findbinary: int->bin
      =fun x lst ->
        match x with 
        0-> lst
        |_-> findbinary (x/2) (lst::(if y mod 2=0 then ZERO else ONE))
      in findbinary a []
  *) 
      ;;




      
      
