(*
* Test file for hw1
*)

use "hw2.sml";

val t1 = all_except_option("a", ["a", "b", "c", "d"]) = SOME(["b", "c", "d"])
val t2 = all_except_option("a", ["a", "b", "c"]) = SOME(["b", "c"])
val t3 = all_except_option("a", ["a"]) = SOME([])
val t4 = all_except_option("d", ["a", "b", "c"]) = NONE

val t5 = get_substitutions1([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Jo") = ["Ce", "Fa", "Ga"]
val t6 = get_substitutions1([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Po") = []

val t7 = get_substitutions2([["Jo", "Ce"], ["a", "b"], ["Fa", "Jo", "Ga"]], "Jo") = ["Ce", "Fa", "Ga"]
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

val t20 = all_same_color([(Spades, Ace), (Clubs, King)]) = true
val t21 = all_same_color([(Hearts, Ace), (Clubs, King)]) = false
val t22 = all_same_color([(Spades, Ace), (Diamonds, King)]) = false
val t23 = all_same_color([(Hearts, Ace), (Diamonds, King)]) = true
val t24 = all_same_color([]) = true
val t25 = all_same_color([(Hearts, Num 2)]) = true
val t26 = all_same_color([(Hearts, Ace), (Diamonds, King), (Clubs, Num 2)]) = false

val t27 = sum_cards([(Spades, Ace), (Clubs, King)]) = 21
val t28 = sum_cards([(Hearts, Num 2)]) = 2
val t29 = sum_cards([(Hearts, Ace), (Diamonds, King), (Clubs, Num 2)]) = 23
val t30 = sum_cards([]) = 0

val t31 = is_present([1, 2, 3], 2) = true
val t32 = is_present([1, 2, 3], 4) = false
val t33 = is_present(["a", "b", "c"], "a") = true
val t34 = is_present(["a", "b", "c"], "4") = false

val t35 = (remove_card([], (Clubs, King), IllegalMove) handle IllegalMove => [(Clubs, King)]) = 
   [(Clubs, King)]
val t36 = (remove_card([(Hearts, Ace), (Clubs, King)], (Spades, Num 2), IllegalMove) 
   handle IllegalMove => [(Clubs, King)]) = [(Clubs, King)]
val t37 = remove_card([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val t38 = remove_card([(Hearts, Ace), (Diamonds, King), (Clubs, Num 2)],
   (Diamonds, King), IllegalMove) = [(Hearts, Ace), (Clubs, Num 2)]
