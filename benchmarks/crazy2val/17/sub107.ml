type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2
let rec crazy2val (cr : crazy2) : int =
match (cr) with
|(NIL) -> 0
|(ZERO  cr1) -> 2*(crazy2val cr1)
|(ONE cr2) -> 1+2*(crazy2val cr2)
|(MONE cr3) -> -1+2*(crazy2val cr3)
