(*
  Department : Electrical Engineering
  Student-Id : 2008-11923
  Name : HyeonIL Choi (최현일)
  Date: 2017-9-13
  Homework-# : 2-3
  Excercise-Name : sum of k-nary number
*)

type crazy2 = NIL 
| ZERO of crazy2 
| ONE of crazy2 
| MONE of crazy2

type carry = CZERO | CONE | CMONE

let crazy2add (crazy2_1, crazy2_2) = (
  let rec crazy2addWithCarry (crazy2_1, crazy2_2, carry) = (
    match carry with
    | CZERO -> (
      match (crazy2_1, crazy2_2) with
      | (NIL, _) -> crazy2_2
      | (_, NIL) -> crazy2_1
      | (MONE crazy2_1_sub, ONE crazy2_2_sub )
      | (ONE crazy2_1_sub, MONE crazy2_2_sub )
      | (ZERO crazy2_1_sub, ZERO crazy2_2_sub ) 
        -> ZERO (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CZERO))
      | (ZERO crazy2_1_sub, ONE crazy2_2_sub )
      | (ONE crazy2_1_sub, ZERO crazy2_2_sub ) 
        -> ONE (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CZERO))
      | (ZERO crazy2_1_sub, MONE crazy2_2_sub )
      | (MONE crazy2_1_sub, ZERO crazy2_2_sub ) 
        -> MONE (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CZERO))
      | (ONE crazy2_1_sub, ONE crazy2_2_sub ) 
        -> ZERO (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CONE))
      | (MONE crazy2_1_sub, MONE crazy2_2_sub ) 
        -> ZERO (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CMONE))
    )
    | CONE -> (
      match (crazy2_1, crazy2_2) with
      | (NIL, _) -> crazy2addWithCarry(ONE NIL, crazy2_2, CZERO)
      | (_, NIL) -> crazy2addWithCarry(crazy2_1, ONE NIL, CZERO)
      | (MONE crazy2_1_sub, ONE crazy2_2_sub )
      | (ONE crazy2_1_sub, MONE crazy2_2_sub )
      | (ZERO crazy2_1_sub, ZERO crazy2_2_sub ) 
        -> ONE (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CZERO))
      | (ZERO crazy2_1_sub, ONE crazy2_2_sub )
      | (ONE crazy2_1_sub, ZERO crazy2_2_sub ) 
        -> ZERO (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CONE))
      | (ZERO crazy2_1_sub, MONE crazy2_2_sub )
      | (MONE crazy2_1_sub, ZERO crazy2_2_sub ) 
        -> ZERO (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CZERO))
      | (ONE crazy2_1_sub, ONE crazy2_2_sub ) 
        -> ONE (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CONE))
      | (MONE crazy2_1_sub, MONE crazy2_2_sub ) 
        -> MONE (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CZERO))
    )
    | CMONE -> (
      match (crazy2_1, crazy2_2) with
      | (NIL, _) -> crazy2addWithCarry(MONE NIL, crazy2_2, CZERO)
      | (_, NIL) -> crazy2addWithCarry(crazy2_1, MONE NIL, CZERO)
      | (MONE crazy2_1_sub, ONE crazy2_2_sub )
      | (ONE crazy2_1_sub, MONE crazy2_2_sub )
      | (ZERO crazy2_1_sub, ZERO crazy2_2_sub ) 
        -> MONE (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CZERO))
      | (ZERO crazy2_1_sub, ONE crazy2_2_sub )
      | (ONE crazy2_1_sub, ZERO crazy2_2_sub ) 
        -> ZERO (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CZERO))
      | (ZERO crazy2_1_sub, MONE crazy2_2_sub )
      | (MONE crazy2_1_sub, ZERO crazy2_2_sub ) 
        -> ZERO (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CMONE))
      | (ONE crazy2_1_sub, ONE crazy2_2_sub ) 
        -> ONE (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CZERO))
      | (MONE crazy2_1_sub, MONE crazy2_2_sub ) 
        -> MONE (crazy2addWithCarry (crazy2_1_sub, crazy2_2_sub, CMONE))
    )
  ) in
  crazy2addWithCarry (crazy2_1,crazy2_2, CZERO) 
)


