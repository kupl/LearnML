(* hw2ex2.ml *)





type crazy2 = NIL 
            | ZERO of crazy2 
            | ONE of crazy2 
            | MONE of crazy2


(* some part of hw2.1 


   (*crazy2val: crazy2 -> int*)



   let rec crazy2len (c: crazy2) : int = 
   match c with
   | NIL -> 0
   | ZERO a -> 1 + (crazy2len a)
   | ONE a -> 1 + (crazy2len a)
   | MONE a -> 1 + (crazy2len a)


   let _ = crazy2len (ZERO(ONE(MONE NIL)))

   let rec crazy2power (p: int) : int = 
   match p with
   | 0 -> 1
   | _ -> 2 * crazy2power (p - 1)

   let _ = crazy2power 3




   let rec crazy2val_aux (c: crazy2) (p: int) : int = 
   match c with
   | NIL -> 0
   | ZERO c_sub -> (crazy2val_aux c_sub (p+1))
   | ONE c_sub -> (crazy2val_aux c_sub (p+1)) + (crazy2power p)
   | MONE c_sub -> (crazy2val_aux c_sub (p+1)) - (crazy2power p) 


   let crazy2val (c: crazy2) : int =
   crazy2val_aux c 0


   let _ = crazy2val (ZERO(ONE(MONE NIL)))

   let _ = crazy2val (ZERO(ONE(ONE(ONE NIL))))


*)


(* follwing is the main hw2ex2 *)

exception CarryBound


(* crazy2add: crazy2 * crazy2 -> crazy2*)
(*crazy2val (crazy2add(z,z')) = crazy2val(z) + crazy2val(z'). *)

let rec carry_oneside (c: crazy2) (carry: int) : crazy2 = 
  match c, carry with
    | NIL, 0 -> (ZERO NIL)
    | NIL, 1 -> (ONE NIL)
    | NIL, -1 -> (MONE NIL)
    | ZERO sub_c, 0 -> (ZERO (carry_oneside sub_c 0))
    | ZERO sub_c, 1 -> (ONE (carry_oneside sub_c 0))
    | ZERO sub_c, -1 -> (MONE (carry_oneside sub_c 0))
    | ONE sub_c, 0 -> (ONE (carry_oneside sub_c 0))
    | ONE sub_c, 1 -> (ZERO (carry_oneside sub_c 1))
    | ONE sub_c, -1 -> (ZERO (carry_oneside sub_c 0))
    | MONE sub_c, 0 -> (MONE (carry_oneside sub_c 0))
    | MONE sub_c, 1 -> (ZERO (carry_oneside sub_c 0))
    | MONE sub_c, -1 -> (ZERO (carry_oneside sub_c (-1)))
    | _, _ -> raise CarryBound



let rec crazy2add_aux (c1: crazy2) (c2: crazy2) (carry: int) : crazy2 = 
  match c1, c2, carry with
    | NIL, NIL, 0 -> (ZERO NIL)
    | NIL, NIL, 1 -> (ONE NIL)
    | NIL, NIL, -1 -> (MONE NIL)
    | NIL, _, _ -> carry_oneside c2 carry
    | _, NIL, _ -> carry_oneside c1 carry
    | ZERO sub1, ZERO sub2, 0 (* nextcarry=0 cases below *)
    | ZERO sub1, ONE sub2, -1
    | ZERO sub1, MONE sub2, 1
    | MONE sub1, ZERO sub2, 1
    | ONE sub1, ZERO sub2, -1 
    | MONE sub1, ONE sub2, 0 -> (ZERO (crazy2add_aux sub1 sub2 0))
    | ZERO sub1, ZERO sub2, 1
    | ZERO sub1, ONE sub2, 0 
    | ONE sub1, ZERO sub2, 0 
    | ONE sub1, MONE sub2, 1 
    | ONE sub1, ONE sub2, -1
    | MONE sub1, ONE sub2, 1 -> (ONE (crazy2add_aux sub1 sub2 0))
    | ONE sub1, MONE sub2, -1
    | ZERO sub1, ZERO sub2, -1
    | ONE sub1, MONE sub2, 0 
    | ZERO sub1, MONE sub2, 0 
    | MONE sub1, ZERO sub2, 0 
    | MONE sub1, ONE sub2, -1 
    | MONE sub1, MONE sub2, 1 -> (MONE (crazy2add_aux sub1 sub2 0))
    | ZERO sub1, ONE sub2, 1 (* nextcarry=1 cases below *)
    | ONE sub1, ZERO sub2, 1
    | ONE sub1, ONE sub2, 0 -> (ZERO (crazy2add_aux sub1 sub2 1))
    | ONE sub1, ONE sub2, 1 -> (ONE (crazy2add_aux sub1 sub2 1))
    | ZERO sub1, MONE sub2, -1 (* nextcarry=-1 cases below *)
    | MONE sub1, ZERO sub2, -1
    | MONE sub1, MONE sub2, 0 -> (ZERO (crazy2add_aux sub1 sub2 (-1)))
    | MONE sub1, MONE sub2, -1 -> (MONE (crazy2add_aux sub1 sub2 (-1)))
    | _,_,_ -> raise CarryBound


let crazy2add (c1,c2) : crazy2 = 
  crazy2add_aux c1 c2 0

(* 

let _ = crazy2add ( (ZERO(ONE(MONE(ONE NIL)))) , (ZERO(ONE(MONE NIL))) )
let _ = crazy2val (ZERO (ZERO (ONE (ONE (ZERO NIL)))))


testcase


let _= 
let print_bool x = print_endline (string_of_bool x) in 

print_bool (0 = (crazy2val (crazy2add (ZERO NIL, ZERO NIL)))); 
print_bool (0 = (crazy2val (crazy2add (MONE NIL, ONE NIL)))); 
print_bool (1 = (crazy2val (crazy2add (ZERO NIL, ONE NIL)))); 
print_bool (4 = (crazy2val (crazy2add (ONE (ONE NIL), ONE NIL)))); 
print_bool (-683 = (crazy2val (crazy2add (MONE (ZERO (ZERO (ZERO NIL))), (ZERO (ONE (ONE (ZERO (MONE (ONE (ONE (ZERO (ONE (ZERO (MONE NIL))))))))))))))) 

*)

