datatype Atom =NIL | SYMBOL of string;
datatype SExp =ATOM of Atom | CONS of (SExp * SExp);

local
  fun parseForAtom "NIL"=ATOM(NIL) (*simple case, only atoms*)
    | parseForAtom element=ATOM(SYMBOL(element));

fun parseForList [] = (parseForAtom "NIL", []) (*main case,  handle list*)
  | parseForList ("(" :: restOfList) =
    let
      val (listExpression, restOfElements) = parseForList restOfList (*current list*)
      val (listOfRestExpression,restAfterList)= parseForList restOfElements (*what comes after*)
    in
      (CONS (listExpression, listOfRestExpression),restAfterList)
    end
  | parseForList (")" :: restOfList) = (parseForAtom "NIL", restOfList) (*end current list*)
  | parseForList (token :: restOfList) = (*element in list*)
    let
      val expression = parseForAtom token
      val (listExpression, restOfElements) = parseForList restOfList
    in
      (CONS (expression, listExpression),restOfElements)
    end
in
  fun parse []=parseForAtom "NIL" (*empty list*)
    | parse ("("::restOfList)= (*start the list*)
      let
        val (listExpression, restOfElements)=parseForList restOfList
      in
        listExpression
      end
    | parse (token::xs)=parseForAtom token (*simple expression*)
end;
