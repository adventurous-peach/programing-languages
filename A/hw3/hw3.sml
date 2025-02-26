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
*)
fun only_capitals lst =
    List.filter (fn s => (Char.isUpper o String.sub) (s,0)) lst

(*
* string list -> string
* Return the longest string from the given list
* longest_string1(["a", "b", "abc", "db"]) = "abc"
* longest_string1(["abc", "dub", "a"]) = "abc"
* longest_string1([]) = ""
*)
fun longest_string1 lst =
    let
	fun compare(i, s) =
	    if String.size i > String.size s then i else s
    in foldl compare "" lst end
