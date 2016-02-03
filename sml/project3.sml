(* Coursera Programming Languages, Homework 3, Provided Code *)

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
    List.filter (fn x => Char.isUpper(String.sub (x, 0)))

val longest_string1 =
    List.foldl (fn(x, init) => if String.size x > String.size init then x else init) ""

val longest_string2=
 List.foldl (fn(x, init) => if String.size x >= String.size init then x else init) ""

fun longest_string_helper f xs =
    case xs of
	[] => ""
      | x::xs' => 
	let
	    val result = longest_string_helper f xs'
	in
	    if f(String.size x, String.size result) then x else result 
	end

val longest_string3 = longest_string_helper (fn (x,y) => if x > y then true else false)
val longest_string4 = longest_string_helper (fn (x,y) => if x >= y then true else false)

val longest_capitalized =
    longest_string1 o only_capitals

val rev_string = 
  String.implode o List.rev o String.explode

fun first_answer f xs =
    case xs of    
	[] => raise NoAnswer
      | x::xs' => case f x of
		      NONE => first_answer f xs' 
		    | SOME v => v

fun all_answers f xs =
    let
	fun helper (itt, out) =
	    case itt of
		[] => SOME out
	      | i::tt => case f i of
 			     NONE => NONE
			   | SOME y => helper (tt, y @ out)  
    in
	helper (xs,[])
    end

(*Functions with the use of provided data structures*)
fun count_wildcards p =
    g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p =
    count_wildcards p + g (fn () => 0) (fn x => String.size x) p
	
fun count_some_var(str, p) =
    g (fn () => 0) (fn x => if (str = x) then 1 else 0) p

fun check_pat p = 
    let
	fun list_all_var_strings p =
	    case p of
		Variable s => [s]
	      | TupleP ps         => List.foldl (fn (p,xs) => (list_all_var_strings p) @ xs) [] ps
	      | ConstructorP(_,p) => list_all_var_strings p
	      | _                 => []				 
	fun check_equality xs =
	    case xs of
		[] => true
	      | x::[] => true 
	      | x::xs' => if List.exists (fn y => y = x) xs' then false else check_equality xs'
    in
	check_equality (list_all_var_strings p)				      
    end

fun match pair =
  case pair of
      (_, Wildcard) => SOME []
    | (v, Variable s) => SOME [(s,v)]
    | (Unit, UnitP) => SOME []  
    | (Const x', ConstP x) => if x = x' then SOME [] else NONE
    | (Tuple ps', TupleP ps) => if List.length ps = List.length ps' 
				then all_answers match (ListPair.zip(ps', ps))
				else NONE
    | (Constructor(s2, v), ConstructorP (s1, p)) => if s1 = s2 then match (v, p) else NONE
    | _ => NONE

fun first_match v ptts =
    SOME (first_answer (fn x =>  match (v, x)) ptts)
    handle NoAnswer => NONE
