(*first part*)
datatype Atom = SYMBOL of string | NIL;
datatype SExp = ATOM of Atom | CONS of (SExp * SExp);

exception Undefined;
exception Empty;

(* TODO *)

(*second part*)
fun initEnv () : string -> SExp = fn exp => raise Undefined;
(*third part*)
fun define str oldEnv valToBind=fn exp:string=> if str=exp then valToBind else oldEnv exp;

(*fourth part*)
  fun emptyNestedEnv()=[initEnv()];

(*fifth part*)
fun pushEnv element oldEnv=element::oldEnv;
fun popEnv [] = raise Empty
  | popEnv (x::xs)=xs;
fun topEnv [] = raise Empty
  | topEnv (x::xs)=x;

(*sixth part*)
fun defineNested _ [] _ = raise Empty
  | defineNested str stack valToBind=(define str (topEnv stack) valToBind)::popEnv stack;

(*seventh part*)
fun find "a" [] = raise Undefined
  | find _ [] = raise Undefined
  | find str (x::xs) = x str handle Undefined=> find str xs;
