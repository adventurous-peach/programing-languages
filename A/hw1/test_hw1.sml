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
