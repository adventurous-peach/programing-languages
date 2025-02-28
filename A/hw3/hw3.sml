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

val only_capitals =
    List.filter (fn s => (Char.isUpper o String.sub) (s,0))

val longest_string1 =
    foldl (fn (i, s) => if String.size i > String.size s then i else s) ""

fun longest_string2 lst =
    foldl (fn (i, s) => if String.size i >= String.size s then i else s) "" lst

fun longest_string_helper f lst =
    foldl (fn (i, s) => if f(String.size i,String.size s) then i else s) "" lst

val longest_string3 = longest_string_helper (fn (i, s) => i > s)

val longest_string4 = longest_string_helper (fn (i, s) => i >= s)

val longest_capitalized = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f lst =
    case lst of
	 [] => raise NoAnswer
       | x::xs => case f x of
		       NONE => first_answer f xs
		     | SOME v => v

fun all_answers f lst =
    let 
	fun help(lst, acc) =
	    case lst of
		  [] => SOME acc
		| x::xs => case f x of
			  NONE => NONE
			| SOME v => help(xs, acc @ v)
    in help(lst, []) end

val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) (fn s => String.size s)

fun count_some_var(s, p) = 
    g (fn _ => 0) (fn e => if e = s then 1 else 0) p

fun check_pat p =
    let 
	fun get_vars p =
	    case p of
		  Variable s => [s]
		| TupleP lst => List.foldl (fn (e,acc) => (get_vars e) @ acc) [] lst
		| ConstructorP (_, p) => get_vars p
		| _ => []
	fun check_repeats lst =
	    case lst of
		  [] => true
		| x::xs => if List.exists (fn e => x = e) xs
			   then false
			   else check_repeats xs
    in (check_repeats o get_vars) p end

fun match arg =
    case arg of
	 (_, Wildcard) => SOME []
       | (v, Variable s) => SOME[(s, v)]
       | (Const a, ConstP b) => if a = b then SOME [] else NONE
       | (Unit, UnitP) => SOME []
       | (Tuple l, TupleP l') => all_answers match (ListPair.zipEq(l, l') handle UnequalLengths => [(Unit,ConstP 0)])
       | (Constructor(s, v),ConstructorP(s',p)) => if s = s' 
						   then match(v, p)
						   else NONE
       | _ => NONE

fun first_match(v, ps) =
    (SOME (first_answer (fn e => match(v, e)) ps)) handle NoAnswer => NONE
