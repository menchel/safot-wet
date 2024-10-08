type regexp = string;

local
    fun multiStar [] = false (*empty*)
    | multiStar [x] = false (*only one*)
    | multiStar (x::y::xs) = (*check for two*)
        if x = #"*" 
        then
                if y = #"*" then true
                else multiStar (y::xs)
        else multiStar (y::xs);

    fun isMatchHelper [] [] = true (*both empty*)
    | isMatchHelper []  _ = false (*only regexp empty*)
    | isMatchHelper [#"*"] [] =false (*strarts with * *)
    | isMatchHelper (regFirst :: #"*" :: regLast) [] =isMatchHelper regLast [] (*starts with letter* *)
    | isMatchHelper (regFirst::regRest) [] =false (*starts with letter+letter or letter+empty *)
    | isMatchHelper (regFirst::regRest) (stringFirst::stringRest) =
        if regFirst = #"*" then false
        else
                if List.null regRest 
                then
                    if regFirst = stringFirst then isMatchHelper regRest stringRest else false (*same character*)
                else
                    if hd regRest = #"*" 
                    then
                        if isMatchHelper (tl regRest) (stringFirst::stringRest) then true (*take zero of letter*)
                        else 
                            if regFirst = stringFirst then isMatchHelper (regFirst::regRest) stringRest else false (*take at least one of letter*)
                    else
                        if regFirst= stringFirst then isMatchHelper regRest stringRest else false;
in
    fun isMatch regexp stringToCheck = if multiStar (String.explode regexp) then false else isMatchHelper (String.explode regexp) (String.explode stringToCheck)
end;