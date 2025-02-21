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

val t15 = dates_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 5) = [(12, 5, 23)]
val t16 = dates_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 6) = []
val t17 = dates_in_month([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], 2) = [(1, 2, 3), (3, 2, 24)]
val t18 = dates_in_month([], 12) = []

val t19 = dates_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], [5, 3]) = [(12, 5, 23)]
val t20 = dates_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], [6, 12, 7]) = []
val t21 = dates_in_months([(1, 2, 3), (3, 4, 30), (3, 2, 24), (12, 5, 23), (3, 9, 27)], [4, 5]) = [(3, 4, 30), (12, 5, 23)]
val t22 = dates_in_months([], [12, 4, 5, 2]) = []

val t23 = get_nth(["a", "b", "c", "d", "e"], 5) = "e"
val t24 = get_nth(["a", "b", "c", "d", "e"], 3) = "c"
val t25 = get_nth(["a", "b", "c", "d", "e"], 1) = "a"

val t26 = date_to_string(2000, 6, 8) = "June 8, 2000"
val t27 = date_to_string(1996, 8, 1) = "August 1, 1996"
val t28 = date_to_string(2025, 2, 24) = "February 24, 2025"
