datatype 'a DSeq = DNil | DCons of 'a * (unit -> 'a DSeq) * (unit -> 'a DSeq);
datatype 'a Seq = Nil | Cons of 'a * (unit -> 'a Seq);

fun coords (3, _) = DNil
  | coords (_, 3) = DNil
  | coords (x, y) = DCons((x, y), fn () => coords (x + 1, y), fn () => coords (x, y + 1));
  
val s = coords (0, 0);

fun next Nil = Nil
  | next (Cons(x, xf)) = xf ();
  
fun take s 0 = []
  | take Nil _ = []
  | take (Cons (x, xf)) n = x :: take (xf ()) (n - 1);

fun pcoords (3, _) = DNil
  | pcoords (_, 3) = DNil
  | pcoords (x, y) = (
    print ("exec: (" ^ Int.toString x ^ ", " ^  Int.toString y ^ ")\n"); 
    DCons((x, y), fn () => pcoords (x, y + 1), fn () => pcoords (x + 1,y))
  );
  
val p = pcoords (0, 0);
(* part 1 *)

local
   fun createRow DNil _ = []
    | createRow _ 0 = []
    | createRow (DCons (elem, _, next)) len = [elem] @ (createRow (next()) (len-1))

   fun toMatrixHelper DNil (_,_ ) result = result
    | toMatrixHelper _ (0,_) result = result
    | toMatrixHelper (DCons (elem, next, nextCol)) (rows,cols) result = 
        let
          val newResult = result@[(createRow (DCons (elem, next, nextCol)) cols)]
        in
          toMatrixHelper (next()) ((rows-1),cols) newResult
        end
in
  fun toMatrix dSeq (rows,cols) = toMatrixHelper dSeq (rows,cols) [];
end

(* part 2 *)

local
    fun QHelper up down =
        DCons(
            (up,down),
            fn () => (QHelper up (down+1)),
            fn () => (QHelper (up+1) down)
        )
in
    fun Q ()= QHelper 1 1;
end

(* part 3 *)
(*
it is basically bfs
*)
local
    fun addElements [] [] _ = Nil
      | addElements [] nextQueue _ = addElements nextQueue [] 1
      | addElements (x::xs) nextQueue isFirst =
        let
            val dSeq = x()
        in
            if isFirst=1 then
                case dSeq of
                    DNil => addElements xs nextQueue 0
                | (DCons(element,down,right)) => Cons(element, fn()=> addElements xs (nextQueue@[right,down]) 0)
            else
                case dSeq of
                    DNil => addElements xs nextQueue 0
                | (DCons(element,down,right)) => Cons(element, fn()=> addElements xs (nextQueue@[down]) 0)
        end
in
    fun diags DNil = Nil
      | diags (DCons(element,down,right)) = Cons(element, fn()=> addElements [right,down] [] 1);
end