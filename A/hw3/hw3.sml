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
