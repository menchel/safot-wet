(*first part*)
fun to_binary 0=[]
  | to_binary 1=[1]
  | to_binary a=if (a mod 2)=0 then [0]@ to_binary(a div 2) else [1]@ to_binary(((a-1) div 2));

(*second part*)
local
  fun countVector [] (zeroes,ones)=(zeroes,ones)
    | countVector (x::xs) (zeroes,ones)=
        if x=0 then countVector xs (zeroes+1,ones)
        else countVector xs (zeroes,ones+1);
  fun createEncode [] returnList _ size=returnList@to_binary(size)
    | createEncode (x::xs) returnList (zeroes,ones) size=
        if zeroes=ones then returnList@[x]@xs@to_binary(size)
        else
            if x=0 then createEncode xs (returnList@[1]) (zeroes-1,ones+1) (size+1)
            else createEncode xs (returnList@[0]) (zeroes+1,ones-1) (size+1);
  fun encodeHelper list=createEncode list [] (countVector list (0,0)) 0
in
    fun encode list=encodeHelper list
end;

(*third part*)
local
    fun reverse_to_binary [] _ current=current
      | reverse_to_binary [1] multi current=multi+current
      | reverse_to_binary (x::xs) multi current=
      if x=0 then reverse_to_binary xs (2*multi) current
      else reverse_to_binary xs (2*multi) (current+multi);
    fun getRestOfList [] _ _=[]
      | getRestOfList (x::xs) ending index=
        if index<ending then getRestOfList xs ending (index+1)
        else x::xs;
    fun createDecode [] _ _ _=[]
      | createDecode (x::xs) size placeToStopReplace currIndex=
        if currIndex<size then
            if currIndex<placeToStopReplace then
                if x=0 then [1]@(createDecode xs size placeToStopReplace (currIndex+1))
                else [0]@(createDecode xs size placeToStopReplace (currIndex+1))
            else
                [x]@(createDecode xs size placeToStopReplace (currIndex+1))
        else
            [];
in
    fun decode (list,length)=createDecode list (length) (reverse_to_binary (getRestOfList list length 0) 1 0) 0;
end;

decode ([0,1,0,1,1], 4);
decode ([0,1,0,0,1,1,0,0,1], 6);
decode ([1,0,0,1,1], 4);
decode ([1,0,1,1,0,0,0,0,1], 6);
decode ([0,1,1,1,0,0,0,0,1], 6);
decode ([0,1,1], 2);
