use "hw4_q1.sml";

(* TODO, REPLACE WITH PATH TO YOUR PARSER *)
use "hw3_q3.sml";
(* end of parser*)

exception LispError;

(* Helper function - feel free to delete *)
fun first (x, _) = x;

local
    fun tokenize x = 
        String.tokens (fn c: char => c = #" ") 
            (String.translate (fn #"(" => "( " | #")" => " )" | c => str c) x);

    (* Helper functions - feel free to delete *)
    (* ====================================== *)
    fun is_digit c = c >= #"0" andalso c <= #"9";

    fun is_number str =
        let
            fun check [] = true
            | check (c::cs) = is_digit c andalso check cs
            
            val chars = String.explode str
        in
            if List.null chars then false else check chars
        end;
        
    fun char_to_int c = ord(c) - ord(#"0")

    fun string_to_int str =
        let
            fun convert [] acc = acc
            | convert (c::cs) acc = convert cs (10 * acc + char_to_int c)
        in
            convert (String.explode str) 0
        end;

    fun sexp_to_int sexp =
        case sexp of
            ATOM (SYMBOL s) => string_to_int s
          | _ => raise LispError;
    (* ====================================== *)
    fun evalAux (ATOM(NIL)) env = (ATOM(NIL),env)
      | evalAux (ATOM(SYMBOL "t")) env = (ATOM(SYMBOL "t"),env)
      | evalAux (ATOM(SYMBOL symbol)) env =
        (if is_number symbol then (ATOM(SYMBOL symbol),env)
         else 
            let
                val arg = (find symbol env) handle Undefined => raise LispError
                val (result, new_env) = evalAux arg env
            in
                (result, new_env)
            end)
      | evalAux (CONS((ATOM(SYMBOL ("cons"))),second)) env = 
        (case second of
            CONS(firstElement,CONS(secondElement,ATOM(NIL))) =>
            let
                val (firstVal, _)=evalAux firstElement env
                val (secondVal, _)=evalAux secondElement env
            in
                (CONS(firstVal,secondVal),env)
            end
        | _ => raise LispError)
      | evalAux (CONS((ATOM(SYMBOL ("car"))),second)) env = 
            (case second of
                CONS(a,_) =>
                    (let                        
                        val (evaluation,_) = evalAux a env
                    in
                        case evaluation of
                            CONS(car,_) => (car,env)
                        | _ => raise LispError
                    end)
            | _ => raise LispError)
      | evalAux (CONS((ATOM(SYMBOL ("cdr"))),second)) env = 
           (case second of
                CONS(a,_) =>
                    (let                        
                        val (evaluation,_) = evalAux a env
                    in
                        case evaluation of
                            CONS(_,cdr) => (cdr,env)
                        | _ => raise LispError
                    end)
            | _ => raise LispError)
      | evalAux (CONS((ATOM(SYMBOL ("eq"))),second)) env =
            (case second of
                CONS(first,CONS(secondTwo,ATOM(NIL))) =>
                    let
                        val (firstElem, _)= evalAux first env
                        val (secondElem, _)= evalAux secondTwo env
                    in
                        case firstElem of
                            CONS(_,_) => (ATOM(NIL),env)
                        |   exp1 =>
                            (
                                case secondElem of
                                    CONS(_,_) => (ATOM(NIL),env)
                                |   exp2 => if exp1=exp2 then (ATOM(SYMBOL "t"),env) else (ATOM(NIL),env)
                            )
                    end
            | _ => raise LispError)
      | evalAux (CONS((ATOM(SYMBOL ("atom"))),second)) env =
            (case second of
                CONS(secondVal,ATOM(NIL)) =>
                    let
                        val (value, _) = evalAux secondVal env
                    in
                        case value of
                            ATOM(a) => (ATOM(SYMBOL "t"),env)
                        | _ => (ATOM(NIL),env)
                    end
            | _ => raise LispError)
        | evalAux (CONS((ATOM(SYMBOL ("quote"))),second)) env =
            (case second of
                CONS(secondVal,_) => (secondVal,env)
            | _ => raise LispError)
        | evalAux (CONS((ATOM(SYMBOL ("cond"))),second)) env = cond_helper second env
        | evalAux (CONS((ATOM(SYMBOL ("null"))),ATOM(a))) env =if a=NIL then (ATOM(SYMBOL "t"),env) else (ATOM(NIL),env)
        | evalAux (CONS((ATOM(SYMBOL ("null"))),CONS(real,rest))) env = 
        (
            let
                val (valOfExp,_) = evalAux real env
            in
                if valOfExp=ATOM(NIL) then (ATOM(SYMBOL "t"),env) else (ATOM(NIL),env)
            end
        )
        | evalAux (CONS((ATOM(SYMBOL ("+"))),CONS(op1,CONS(op2,_)))) env = operator_helper (ATOM(SYMBOL ("+"))) op1 op2 env
        | evalAux (CONS((ATOM(SYMBOL ("-"))),CONS(op1,CONS(op2,_)))) env = operator_helper (ATOM(SYMBOL ("-"))) op1 op2 env
        | evalAux (CONS((ATOM(SYMBOL ("*"))),CONS(op1,CONS(op2,_)))) env = operator_helper (ATOM(SYMBOL ("*"))) op1 op2 env
        | evalAux (CONS((ATOM(SYMBOL ("/"))),CONS(op1,CONS(op2,_)))) env = operator_helper (ATOM(SYMBOL ("/"))) op1 op2 env
        | evalAux (CONS((ATOM(SYMBOL ("mod"))),CONS(op1,CONS(op2,_)))) env = operator_helper (ATOM(SYMBOL ("mod"))) op1 op2 env
        | evalAux (CONS((ATOM(SYMBOL ("="))),CONS(op1,CONS(op2,_)))) env = equality_helper (ATOM(SYMBOL ("="))) op1 op2 env
        | evalAux (CONS((ATOM(SYMBOL ("/="))),CONS(op1,CONS(op2,_)))) env = equality_helper (ATOM(SYMBOL ("/="))) op1 op2 env
        | evalAux (CONS((ATOM(SYMBOL ("<"))),CONS(op1,CONS(op2,_)))) env = equality_helper (ATOM(SYMBOL ("<"))) op1 op2 env
        | evalAux (CONS((ATOM(SYMBOL (">"))),CONS(op1,CONS(op2,_)))) env = equality_helper (ATOM(SYMBOL (">"))) op1 op2 env
        | evalAux (CONS(CONS((ATOM(SYMBOL "lambda")),(CONS(param_names,(CONS(body,ATOM(NIL)))))),parameters)) env = lambda_helper param_names body parameters env env
        | evalAux (CONS(CONS((ATOM(SYMBOL "label")),(CONS((ATOM(SYMBOL(func_name))),(CONS(func,ATOM(NIL)))))),parameters)) env =
            (
                let
                    val bind = defineNested func_name env func
                    val newExp = CONS((ATOM(SYMBOL(func_name))),parameters)
                    val (result,_) = evalAux newExp bind
                in
                    (result,env)
                end
            )
        | evalAux (CONS((ATOM(SYMBOL func_alias)),parameters)) env =
            (
                let
                    val arg = (find func_alias env) handle Undefined => raise LispError
                    val updatedSExp = (CONS(arg,parameters))
                in
                    case updatedSExp of
                        (CONS(CONS((ATOM(SYMBOL "lambda")),(CONS(param_names,(CONS(body,ATOM(NIL)))))),parameters)) => label_defined_helper param_names body parameters env env
                    | _ => raise LispError
                end
            )
        | evalAux _ _ = raise LispError
        handle Div => raise LispError

    and cond_helper exp env =
    (
        case exp of
            CONS(current,next) =>
                (case current of
                    CONS(condition,CONS(value,ATOM(NIL))) =>
                    (
                        let
                            val (result,_) = evalAux condition env 
                        in
                            if result<>ATOM(NIL) then (evalAux value env ) else  cond_helper next env
                        end
                    )
                | _ => (ATOM(NIL),env))
        | _ => (ATOM(NIL),env)
    )
    and operator_helper operator first second env =
        let
            val (temp1,_) = (evalAux first env)
            val (temp2,_) = (evalAux second env)
            val valOfFirst = sexp_to_int temp1
            val valOfSecond = sexp_to_int temp2
        in
            case operator of
                ATOM(SYMBOL "+") => (ATOM(SYMBOL (Int.toString(valOfFirst+valOfSecond))),env)
            |   ATOM(SYMBOL "-") => (ATOM(SYMBOL (Int.toString(valOfFirst-valOfSecond))),env)
            |   ATOM(SYMBOL "*") => (ATOM(SYMBOL (Int.toString(valOfFirst*valOfSecond))),env)
            |   ATOM(SYMBOL "/") => (ATOM(SYMBOL (Int.toString(valOfFirst div valOfSecond))),env)
            |   ATOM(SYMBOL "mod") => (ATOM(SYMBOL (Int.toString(valOfFirst mod valOfSecond))),env)
            | _ => raise LispError
        end
    and equality_helper equal first second env =
        let
            val (temp1,_) = (evalAux first env)
            val (temp2,_) = (evalAux second env)
            val valOfFirst = sexp_to_int temp1
            val valOfSecond = sexp_to_int temp2
        in
            case equal of
                ATOM(SYMBOL "=") => if valOfFirst=valOfSecond then (ATOM(SYMBOL "t"),env) else (ATOM(NIL),env)
            |   ATOM(SYMBOL "/=") => if valOfFirst<>valOfSecond then (ATOM(SYMBOL "t"),env) else (ATOM(NIL),env)
            |   ATOM(SYMBOL "<") => if valOfFirst<valOfSecond then (ATOM(SYMBOL "t"),env) else (ATOM(NIL),env)
            |   ATOM(SYMBOL ">") => if valOfFirst>valOfSecond then (ATOM(SYMBOL "t"),env) else (ATOM(NIL),env)
            | _ => raise LispError
        end
    and lambda_helper param_names body param original_env upt_env = 
    (
        case (param_names, param) of
            (ATOM(NIL),ATOM(NIL)) =>
            (
                let
                    val (result,_) = evalAux body upt_env
                in
                    (result,original_env)
                end
            )
        | ((ATOM(NIL)),_) => raise LispError
        | (_,(ATOM(NIL))) => raise LispError
        |((ATOM(SYMBOL(curr1))),(ATOM(SYMBOL(curr2)))) =>
        (
            let
                val bind = defineNested curr1 upt_env (ATOM(SYMBOL(curr2)))
            in
                lambda_helper (ATOM(NIL)) body (ATOM(NIL)) original_env bind
            end
        )
        | (CONS((ATOM(SYMBOL(curr1))),rest1),CONS(curr2,rest2)) =>
        (
            let
                val bind = defineNested curr1 upt_env curr2
            in
                lambda_helper rest1 body rest2 original_env bind
            end
        )
        | _ => raise LispError
    )
    and label_defined_helper param_names body param original_env upt_env = 
    (
        case (param_names, param) of
            (ATOM(NIL),ATOM(NIL)) =>
            (
                let
                    val (result,_) = evalAux body upt_env
                in
                    (result,original_env)
                end
            )
        | ((ATOM(NIL)),_) => raise LispError
        | (_,(ATOM(NIL))) => raise LispError
        |((ATOM(SYMBOL(curr1))),(ATOM(SYMBOL(curr2)))) =>
        (
            let
                val (typeParam,_) = evalAux (ATOM(SYMBOL(curr2))) original_env
                val bind = defineNested curr1 upt_env typeParam
            in
                lambda_helper (ATOM(NIL)) body (ATOM(NIL)) original_env bind
            end
        )
        | (CONS((ATOM(SYMBOL(curr1))),rest1),CONS(curr2,rest2)) =>
        (
            let
                val (typeParam,_) = evalAux curr2 original_env
                val bind = defineNested curr1 upt_env typeParam
            in
                lambda_helper rest1 body rest2 original_env bind
            end
        )
        | _ => raise LispError
    )
    fun parsed sexp = parse(tokenize sexp)

in
    fun eval sexp env = 
    let
        val (result, new_env) = (evalAux (parsed sexp) env) handle LispError => (ATOM (SYMBOL "lisp-error"), env)
    in
        (result, new_env)
    end
end;
