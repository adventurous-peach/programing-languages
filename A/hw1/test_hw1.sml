(*
* Tests for hw1.
*)

use "hw1.sml";

val t1 = is_older((1, 2, 3), (1, 2, 4)) = true
val t2 = is_older((1, 2, 3), (1, 2, 2)) = false
val t3 = is_older((1, 2, 3), (1, 3, 3)) = true
val t4 = is_older((1, 4, 3), (1, 3, 3)) = false
val t5 = is_older((2, 4, 3), (1, 3, 3)) = false
val t6 = is_older((1, 3, 3), (1, 3, 3)) = false

val t7 = number_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 5) = 1
val t8 = number_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 6) = 0
val t9 = number_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 2) = 2
val t10 = number_in_month([], 12) = 0

val t11 = number_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], [5, 3]) = 1
val t12 = number_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)],[6, 12, 7]) = 0
val t13 = number_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], [4, 5]) = 2
val t14 = number_in_months([], [12, 4, 5, 2]) = 0
