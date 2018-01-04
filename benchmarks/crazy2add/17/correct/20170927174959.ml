(*컴퓨터공학부 2014-16775 김민지
programming language hw 2-3*)

type crazy2 = NIL | ZERO of crazy2 | ONE of crazy2 | MONE of crazy2


let rec crazy2overflow ((x:crazy2), (y:crazy2), (z:crazy2)) : crazy2 = 
  match (x, y, z) with
  |(NIL, NIL, _) -> z
  |(NIL, _, NIL) -> y
  |(_, NIL, NIL) -> x
  |(NIL, ONE y1, ONE z1) -> ZERO(crazy2overflow(NIL, y1, ONE NIL))
  |(NIL, ONE y1, ZERO z1) -> ONE(crazy2overflow(NIL, y1, ZERO NIL))
  |(NIL, ONE y1, MONE z1) -> ZERO(crazy2overflow(NIL, y1, ZERO NIL))
  |(NIL, ZERO y1, ONE z1) -> ONE(crazy2overflow(NIL, y1, ZERO NIL))
  |(NIL, ZERO y1, ZERO z1) -> ZERO(crazy2overflow(NIL, y1, ZERO NIL))
  |(NIL, ZERO y1, MONE z1) -> MONE(crazy2overflow(NIL, y1, ZERO NIL))
  |(NIL, MONE y1, ONE z1) -> ZERO(crazy2overflow(NIL, y1, ZERO NIL))
  |(NIL, MONE y1, ZERO z1) -> MONE(crazy2overflow(NIL, y1, ZERO NIL))
  |(NIL, MONE y1, MONE z1) -> ZERO(crazy2overflow(NIL, y1, MONE NIL))
  |(ONE x1, NIL, ONE z1) -> ZERO(crazy2overflow(x1, NIL, ONE NIL))
  |(ONE x1, NIL, ZERO z1) -> ONE(crazy2overflow(x1, NIL, ZERO NIL))
  |(ONE x1, NIL, MONE z1) -> ZERO(crazy2overflow(x1, NIL, ZERO NIL))
  |(ONE x1, ONE y1, NIL) -> ZERO(crazy2overflow(x1, y1, ONE NIL))
  |(ONE x1, ONE y1, ONE z1) -> ONE(crazy2overflow(x1, y1, ONE NIL))
  |(ONE x1, ONE y1, ZERO z1) -> ZERO(crazy2overflow(x1, y1, ONE NIL))
  |(ONE x1, ONE y1, MONE z1) -> ONE(crazy2overflow(x1, y1, ZERO NIL))
  |(ONE x1, ZERO y1, NIL) -> ONE(crazy2overflow(x1, y1, NIL))
  |(ONE x1, ZERO y1, ONE z1) -> ZERO(crazy2overflow(x1, y1, ONE NIL))
  |(ONE x1, ZERO y1, ZERO z1) -> ONE(crazy2overflow(x1, y1, ZERO NIL))
  |(ONE x1, ZERO y1, MONE z1) -> ZERO(crazy2overflow(x1, y1, ZERO NIL))
  |(ONE x1, MONE y1, NIL) -> ZERO(crazy2overflow(x1, y1, NIL))
  |(ONE x1, MONE y1, ONE z1) -> ONE(crazy2overflow(x1, y1, ZERO NIL))
  |(ONE x1, MONE y1, ZERO z1) -> ZERO(crazy2overflow(x1, y1, ZERO NIL))
  |(ONE x1, MONE y1, MONE z1) -> MONE(crazy2overflow(x1, y1, ZERO NIL))
  |(ZERO x1, NIL, ONE z1) -> ONE(crazy2overflow(x1, NIL, ZERO NIL))
  |(ZERO x1, NIL, ZERO z1) -> ZERO(crazy2overflow(x1, NIL, ZERO NIL))
  |(ZERO x1, NIL, MONE z1) -> MONE(crazy2overflow(x1, NIL, ZERO NIL))
  |(ZERO x1, ONE y1, NIL) -> ONE(crazy2overflow(x1, y1, NIL))
  |(ZERO x1, ONE y1, ONE z1) -> ZERO(crazy2overflow(x1, y1, ONE NIL))
  |(ZERO x1, ONE y1, ZERO z1) -> ONE(crazy2overflow(x1, y1, ZERO NIL))
  |(ZERO x1, ONE y1, MONE z1) -> ZERO(crazy2overflow(x1, y1, ZERO NIL))
  |(ZERO x1, ZERO y1, NIL) -> ZERO(crazy2overflow(x1, y1, NIL))
  |(ZERO x1, ZERO y1, ONE z1) -> ONE(crazy2overflow(x1, y1, ZERO NIL))
  |(ZERO x1, ZERO y1, ZERO z1) -> ZERO(crazy2overflow(x1, y1, ZERO NIL))
  |(ZERO x1, ZERO y1, MONE z1) -> MONE(crazy2overflow(x1, y1, ZERO NIL))
  |(ZERO x1, MONE y1, NIL) -> MONE(crazy2overflow(x1, y1, NIL))
  |(ZERO x1, MONE y1, ONE z1) -> ZERO(crazy2overflow(x1, y1, ZERO NIL))
  |(ZERO x1, MONE y1, ZERO z1) -> MONE(crazy2overflow(x1, y1, ZERO NIL))
  |(ZERO x1, MONE y1, MONE z1) -> ZERO(crazy2overflow(x1, y1, MONE NIL))
  |(MONE x1, NIL, ONE z1) -> ZERO(crazy2overflow(x1, NIL, ZERO NIL))
  |(MONE x1, NIL, ZERO z1) -> MONE(crazy2overflow(x1, NIL, ZERO NIL))
  |(MONE x1, NIL, MONE z1) -> ZERO(crazy2overflow(x1, NIL, MONE NIL))
  |(MONE x1, ONE y1, NIL) -> ZERO(crazy2overflow(x1, y1, NIL))
  |(MONE x1, ONE y1, ONE z1) -> ONE(crazy2overflow(x1, y1, ZERO NIL))
  |(MONE x1, ONE y1, ZERO z1) -> ZERO(crazy2overflow(x1, y1, ZERO NIL))
  |(MONE x1, ONE y1, MONE z1) -> MONE(crazy2overflow(x1, y1, ZERO NIL))
  |(MONE x1, ZERO y1, NIL) -> MONE(crazy2overflow(x1, y1, NIL))
  |(MONE x1, ZERO y1, ONE z1) -> ZERO(crazy2overflow(x1, y1, ZERO NIL))
  |(MONE x1, ZERO y1, ZERO z1) -> MONE(crazy2overflow(x1, y1, ZERO NIL))
  |(MONE x1, ZERO y1, MONE z1) -> ZERO(crazy2overflow(x1, y1, MONE NIL))
  |(MONE x1, MONE y1, NIL) -> ZERO(crazy2overflow(x1, y1, MONE NIL))
  |(MONE x1, MONE y1, ONE z1) -> MONE(crazy2overflow(x1, y1, ZERO NIL))
  |(MONE x1, MONE y1, ZERO z1) -> ZERO(crazy2overflow(x1, y1, MONE NIL))
  |(MONE x1, MONE y1, MONE z1) -> MONE(crazy2overflow(x1, y1, MONE NIL))

let rec crazy2add ((x:crazy2), (y:crazy2)) : crazy2 =
  match (x, y) with
  |(NIL, _) -> y
  |(_, NIL) -> x
  |(ZERO x1, ZERO y1) -> ZERO(crazy2add (x1, y1))
  |(ZERO x1, ONE y1) -> ONE(crazy2add (x1, y1))
  |(ZERO x1, MONE y1) -> MONE(crazy2add (x1, y1))
  |(ONE x1, ZERO y1) -> ONE(crazy2add (x1, y1))
  |(ONE x1, ONE y1) -> ZERO(crazy2overflow (x1, y1, (ONE NIL)))
  |(ONE x1, MONE y1) -> ZERO(crazy2add (x1, y1))
  |(MONE x1, ZERO y1) -> MONE(crazy2add (x1, y1))
  |(MONE x1, ONE y1) -> ZERO(crazy2add (x1, y1))
  |(MONE x1, MONE y1) -> ZERO(crazy2overflow (x1, y1, (MONE NIL)))

