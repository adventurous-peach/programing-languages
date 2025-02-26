(* 
* Test file for hw3.
*)

use "hw3.sml";

val t1 = only_capitals(["a", "Hola", "hola", "Perro"]) = ["Hola", "Perro"]
val t2 = only_capitals(["a", "A", "B", "C", "eE", "mSDA"]) = ["A", "B", "C"]

val t3 = longest_string1(["a", "b", "abc", "db"]) = "abc"
val t4 = longest_string1(["abc", "dub", "a"]) = "abc"
val t5 = longest_string1([]) = ""
