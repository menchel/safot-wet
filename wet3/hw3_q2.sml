(*part 1*)
datatype ('a, 'b) heterolist = NIL | ::: of 'a * ('b,'a)heterolist;
infixr 5 :::;

(*part 2*)
fun build4 (x, one, y, two) =x:::(one:::(y:::(two:::NIL)));

(*part 3*)
local

    fun unzipHelperA NIL (first, second) = (first, second)
      | unzipHelperA (x ::: xs) (first, second) = unzipHelperB xs (first@[x], second)
    and unzipHelperB NIL (first, second) = (first, second)
      | unzipHelperB (x ::: xs) (first, second) = unzipHelperA xs (first, second@[x]);
in
    fun unzip heteroList = unzipHelperA heteroList ([], [])
end;

(*part 4*)
fun zip ([],[])=NIL
  | zip ([],_)=raise Empty
  | zip (_,[])=raise Empty
  | zip (x::xs,y::ys)=x:::(y:::zip(xs,ys));

zip ([1, 2, 3, 4, 5], [#"a", #"b", #"c", #"d", #"e"]);