(* 
* Test file for hw3.
*)

use "hw3.sml";

val t0 = only_capitals(["a", "Hola", "hola", "Perro"]) = ["Hola", "Perro"]
val t1 = only_capitals(["a", "A", "B", "C", "eE", "mSDA"]) = ["A", "B", "C"]
val t2 = only_capitals(["a", "s"]) = []

val t3 = longest_string1(["a", "b", "abc", "db"]) = "abc"
val t4 = longest_string1(["abc", "dub", "a"]) = "abc"
val t5 = longest_string1([]) = ""

val t6 = longest_string2(["a", "b", "abc", "db"]) = "abc"
val t7 = longest_string2(["abc", "dub", "a"]) = "dub"
val t8 = longest_string2([]) = ""

val t9 = longest_string3 ["a", "b", "abc", "db"] = "abc"
val t11 = longest_string3 ["abc", "dub", "a"] = "abc"
val t12 = longest_string3 [] = ""

val t13 = longest_string4 ["a", "b", "abc", "db"] = "abc"
val t14 = longest_string4 ["abc", "dub", "a"] = "dub"
val t15 = longest_string4 [] = ""

val t16 = longest_capitalized(["as", "Asd", "Asc", "asD", "DEsd"]) = "DEsd"
val t17 = longest_capitalized(["as", "Asd", "Asc", "asD", "dEsd"]) = "Asd"
val t18 = longest_capitalized(["as", "asd", "sd"]) = ""
val t19 = longest_capitalized([]) = ""

val t20 = rev_string("") = ""
val t21 = rev_string("a") = "a"
val t22 = rev_string("go") = "og"
val t23 = rev_string("hola") = "aloh"

val t24 = first_answer (fn e => if e = "a" then SOME ":P" else NONE) ["e", "a"] = ":P"
val t25 = (first_answer (fn e => if e = "a" then SOME ":P" else NONE) ["e", "a"] 
    handle NoAnswer => ":P") = ":P"
