exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(*
* string list -> string list
* Return the strings from the given list whose char at index 0 is capitalized
* only_capitals(["a", "Hola", "hola", "Perro"]) = ["Hola", "Perro"]
* only_capitals(["a", "A", "B", "C", "eE", "mSDA"]) = ["A", "B", "C"]
* only_capitals(["a", "s"]) = []
*)
fun only_capitals lst =
    List.filter (fn s => (Char.isUpper o String.sub) (s,0)) lst

(*
* string list -> string
* Return the longest string from the given list
* In case of a tie, return the string closest to the begining
* longest_string1(["a", "b", "abc", "db"]) = "abc"
* longest_string1(["abc", "dub", "a"]) = "abc"
* longest_string1([]) = ""
*)
fun longest_string1 lst =
    foldl (fn (i, s) => if String.size i > String.size s then i else s) "" lst

(*
* string list -> string
* Return the longest string from the given list
* In case of a tie, return the string closest to the end
* longest_string2(["a", "b", "abc", "db"]) = "abc"
* longest_string2(["abc", "dub", "a"]) = "dub"
* longest_string2([]) = ""
*)
fun longest_string2 lst =
    foldl (fn (i, s) => if String.size i >= String.size s then i else s) "" lst

(*
* (int * int -> bool) -> string list -> string
* ???
*)
fun longest_string_helper f lst =
    foldl (fn (i, s) => if f(String.size i,String.size s) then i else s) "" lst

(*
* string list -> string
* Return the longest string from the given list
* In case of a tie, return the string closest to the begining
* longest_string3(["a", "b", "abc", "db"]) = "abc"
* longest_string3(["abc", "dub", "a"]) = "abc"
* longest_string3([]) = ""
*)
val longest_string3 = longest_string_helper (fn (i, s) => i > s)

(*
* string list -> string
* Return the longest string from the given list
* In case of a tie, return the string closest to the end
* longest_string4(["a", "b", "abc", "db"]) = "abc"
* longest_string4(["abc", "dub", "a"]) = "dub"
* longest_string4([]) = ""
*)
val longest_string4 = longest_string_helper (fn (i, s) => i >= s)

(* 
* string list -> string
* Return the longest string from a given list whose char at index 0 is capitalized
* longest_capitalized(["as", "Asd", "Asc", "asD", "DEsd"]) = "DEsd"
* longest_capitalized(["as", "Asd", "Asc", "asD", "dEsd"]) = "Asd"
* longest_capitalized(["as", "asd", "sd"]) = ""
* longest_capitalized([]) = ""
*)
val longest_capitalized = longest_string1 o only_capitals

(*
* string -> string
* Return the string s spelled backwards
* rev_string("") = ""
* rev_string("a") = "a"
* rev_string("go") = "og"
* rev_string("hola") = "aloh"
*)
val rev_string = String.implode o List.rev o String.explode

(* 
* ('a -> 'b option) -> a' list -> 'b
* Return v of SOME f(lst) or raise NoAnswer otherwise
* first_answer (fn e => if e = "a" then SOME ":P" else NONE) ["e", "a"] = ":P"
* (first_answer (fn e => if e = "a" then SOME ":P" else NONE) ["e", "a"] handle
* NoAnswer => ":P") = ":P"
*)
fun first_answer f lst =
    case lst of
	 [] => raise NoAnswer
       | x::xs => case f x of
		       NONE => first_answer f xs
		     | SOME v => v

(*
* (a' -> b' option) -> a' list -> b' list
* ???
* all_answers (fn e => if e > 3 then SOME [e] else NONE) [1, 2, 3] = NONE
* all_answers (fn e => if e > 3 then SOME [e] else NONE) [5, 6] = SOME [5, 6]
* all_answers (fn e => if e > 3 then SOME [e] else NONE) [] = SOME []
*)
fun all_answers f lst =
    let 
	fun help(lst, acc) =
	    case lst of
		  [] => SOME acc
		| x::xs => case f x of
			  NONE => NONE
			| SOME v => help(xs, acc @ v)
    in help(lst, []) end


(*
* pattern -> int
* Count how many Wildcard there are in pattern p
* count_wildcards Wildcard = 1 
* count_wildcards (Variable "hola") = 0
* count_wildcards UnitP = 0
* count_wildcards (ConstP 23) = 0
* count_wildcards (TupleP [(Variable "hola"), UnitP]) = 0
* count_wildcards (TupleP [(Variable "hola"), Wildcard]) = 1
* count_wildcards (ConstructorP (":P", ConstP 3)) = 0
* count_wildcards (ConstructorP (":P", Wildcard)) = 1
*)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(*
* pattern -> int
* Count how many Wildcard there are plus the length of all strings s in Variable s of p
* count_wild_and_variable_lengths (TupleP [Variable "hola", ConstP 23, Wildcard,
*   ConstructorP ("P:", TupleP [Variable ":x", Wildcard, Wildcard])]) = 9
* count_wild_and_variable_lengths (Variable "Pero bueno...") = 13
* count_wild_and_variable_lengths Wildcard = 1
* count_wild_and_variable_lengths (ConstP 234) = 0
*)
val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size s)

(*
* Count how many times string s appears in p as Variable s
* count_some_var (":P", Wildcard) = 0
* count_some_var ("s", Variable ":P") = 0
* count_some_var ("o.O", TupleP [Variable "o.O", Variable "o.O", Wildcard,
* ConstP 3, ConstructorP ("o.O", TupleP [Variable "o.O", Wildcard])]) = 3
*)
fun count_some_var(s, p) = 
    g (fn _ => 0) (fn e => if e = s then 1 else 0) p
