(*
* Test file for hw1
*)

use "hw2.sml";

val t1 = all_except_option("a", ["a", "b", "c", "d"]) = SOME(["d", "c", "b"])
val t2 = all_except_option("a", ["a", "b", "c"]) = SOME(["c", "b"])
val t3 = all_except_option("a", ["a"]) = SOME([])
val t4 = all_except_option("d", ["a", "b", "c"]) = NONE

val t5 = get_substitutions1([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Jo") = ["Ce", "Ga", "Fa"]
val t6 = get_substitutions1([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Po") = []
