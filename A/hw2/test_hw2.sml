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

val t7 = get_substitutions2([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Jo") = ["Ga", "Fa", "Ce"]
val t8 = get_substitutions2([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Po") = []

val t9 = card_color(Spades, Queen) = Black
val t10 = card_color(Clubs, Ace) = Black
val t11 = card_color(Diamonds, King) = Red
val t12 = card_color(Hearts, Num 10) = Red

val t13 = card_value(Spades, Queen) = 10
val t14 = card_value(Clubs, Ace) = 11
val t15 = card_value(Hearts, Jack) = 10
val t16 = card_value(Diamonds, King) = 10
val t17 = card_value(Spades, Num 10) = 10
val t18 = card_value(Clubs, Num 2) = 2
val t19 = card_value(Clubs, Num 5) = 5
