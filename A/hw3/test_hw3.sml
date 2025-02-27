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


val t26 = all_answers (fn e => if e > 3 then SOME [e] else NONE) [1, 2, 3] = NONE
val t27 = all_answers (fn e => if e > 3 then SOME [e] else NONE) [5, 6] = SOME [5, 6]
val t28 = all_answers (fn e => if e > 3 then SOME [e] else NONE) [] = SOME []

val t29 = count_wildcards Wildcard = 1 
val t30 = count_wildcards (Variable "hola") = 0
val t31 = count_wildcards UnitP = 0
val t32 = count_wildcards (ConstP 23) = 0
val t33 = count_wildcards (TupleP [(Variable "hola"), UnitP]) = 0
val t34 = count_wildcards (TupleP [(Variable "hola"), Wildcard]) = 1
val t35 = count_wildcards (ConstructorP (":P", ConstP 3)) = 0
val t36 = count_wildcards (ConstructorP (":P", Wildcard)) = 1

val t37 = count_wild_and_variable_lengths (TupleP [Variable "hola", ConstP 23, Wildcard,
    ConstructorP ("P:", TupleP [Variable ":x", Wildcard, Wildcard])]) = 9
val t38 = count_wild_and_variable_lengths (Variable "Pero bueno...") = 13
val t39 = count_wild_and_variable_lengths Wildcard = 1
val t40 = count_wild_and_variable_lengths (ConstP 234) = 0

val t41 = count_some_var (":P", Wildcard) = 0
val t42 = count_some_var ("s", Variable ":P") = 0
val t43 = count_some_var ("o.O", TupleP [Variable "o.O", Variable "o.O", Wildcard, 
    ConstP 3, ConstructorP ("o.O", TupleP [Variable "o.O", Wildcard])]) = 3

(*
val t44 = check_pat (Variable ":O") = true
val t45 = check_pat Wildcard = true
val t46 = check_pat (TupleP [Variable ":x", Variable "S:", Wildcard, ConstP 21, 
    Constructor ("S: > :P", TupleP [Variable ":S", Variable ";)", Wildcard])]) = true
val t47 = check_pat (TupleP [Variable ":x", Variable "S:", Wildcard, ConstP 21, 
    Constructor ("S: > :P", TupleP [Variable "S:", Variable ";)", Wildcard])]) = false
*)
