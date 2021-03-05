exception Eval_error
exception Type_error

type typ = TBool | TInt

type exp = 
    | True
    | False
    | If of exp * exp * exp
    | Num of int
    | IsZero of exp
    | Plus of exp * exp
    | Mult of exp * exp

let rec string_of_typ (t : typ) = match t with
    | TBool -> "TBool"
    | TInt -> "TInt"

let rec string_of_exp (e : exp) = match e with
    | True -> "true"
    | False -> "false"
    | If(e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2
    ^ " else " ^ string_of_exp e3
    | Num(n) -> string_of_int n
    | IsZero(e1) -> "(isZero " ^ string_of_exp e1 ^ ")"
    | Plus(e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
    | Mult(e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"

let rec step (e : exp) = match e with
    | True -> raise Eval_error
    | False -> raise Eval_error
    | If(True, e1, e2) -> e1
    | If(False, e1, e2) -> e2
    | If(e1, e2, e3) -> If(step e1, e2, e3)
    | Num(n) -> raise Eval_error
    | IsZero(Num(0)) -> True
    | IsZero(Num(n)) when n != 0 -> False
    | IsZero(e) -> IsZero(step e)
    | Plus(Num(n1), Num(n2)) -> Num(n1 + n2)
    | Plus(Num(n), e) -> Plus(Num(n), step e)
    | Plus(e1, e2) -> Plus(step e1, e2)
    | Mult(Num(n1), Num(n2)) -> Num(n1 * n2)
    | Mult(Num(n), e) -> Mult(Num(n), step e)
    | Mult(e1, e2) -> Mult(step e1, e2)

let rec multi_step (e : exp) = match e with
    | True -> True
    | False -> False
    | Num(n) -> Num(n)
    | _ -> let e' = step e in multi_step(e')

let rec type_check (e : exp) = match e with
    | True -> TBool
    | False -> TBool
    | If(e1, e2, e3) -> (let e1' = multi_step e1 in
    if (e1' == True) then match multi_step e2 with
        | True -> TBool
        | False -> TBool
        | Num(n) -> TInt
        | _ -> raise Type_error
    else if (e1' == False) then match multi_step e3 with
        | True -> TBool
        | False -> TBool
        | Num(n) -> TInt
        | _ -> raise Type_error
    else raise Type_error)
    | Num(n) -> TInt
    | IsZero(e) -> (match multi_step e with
        | Num(n) -> TBool
        | _ -> raise Type_error)
    | Plus(e1, e2) -> (match multi_step e1, multi_step e2 with
        | Num(n1), Num(n2) -> TInt
        | _ -> raise Type_error)
    | Mult(e1, e2) -> (match multi_step e1, multi_step e2 with
        | Num(n1), Num(n2) -> TInt
        | _ -> raise Type_error)

let () =
    print_endline("== Small-Step Semantics ==");
    print_endline("1) " ^ string_of_exp (multi_step True));
    print_endline("2) " ^ string_of_exp (multi_step False));
    print_endline("3) " ^ string_of_exp (multi_step (Num 0)));
    print_endline("4) " ^ string_of_exp (multi_step (IsZero (Num 0))));
    print_endline("5) " ^ string_of_exp (multi_step (IsZero (Plus (Num 1,
    Num 1)))));
    print_endline("6) " ^ string_of_exp (multi_step (IsZero (Plus (Plus (Num 2,
    Num (-1)), Num 1)))));
    print_endline("7) " ^ string_of_exp (multi_step (Plus (Plus (Num (-1),
    Num 1), Plus (Num (-1), Num 1)))));
    print_endline("8) " ^ string_of_exp (multi_step (Plus (Num (-1), Plus
    (Mult(Num 2, Num 2), Num 1)))));
    print_endline("9) " ^ string_of_exp (multi_step (Plus (Plus (Plus (Num 2,
    Num (-1)), Num 1), Num (-1)))));

    print_endline("10) Eval_error");
    (*print_endline("10) " ^ string_of_exp (multi_step (Plus (IsZero (Plus
    (Num (-1), Num 1)), Num 1))));*)

    print_endline("11) Eval_error");
    (*print_endline("11) " ^ string_of_exp (multi_step (IsZero (If (IsZero 
    (Num 0), True, Num 0)))));*)

    print_endline("12) Eval_error");
    (*print_endline("12) " ^ string_of_exp (multi_step
    (IsZero
        (If
            ( IsZero (Mult (Num 5, Num 0))
            , If (False, Num 0, IsZero (Plus (Num (-1), Num 0)))
            , Num 0 )))));*)

    print_endline("13) " ^ string_of_exp (multi_step (If (IsZero (Plus
    (Num (-1), Num 1)), Num 2, True))));
    print_endline("14) " ^ string_of_exp (multi_step
    (If
        ( If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True)
        , Mult (Num 1, Num 2)
        , True ))));
    print_endline("15) " ^ string_of_exp (multi_step
    (If
        ( If (IsZero (Mult (Num 0, Num 0)), IsZero (Num 2), Num 0)
        , Mult (Num 2, Mult (Num 1, Num 1))
        , Plus
            ( Plus
                ( Plus
                    ( Plus (If (IsZero (Num 0), Num 1, Num 0), Num (-1))
                    , Num 1 )
                , Num (-1) )
            , Num 1 )))));
    print_endline("16) " ^ string_of_exp (multi_step
    (If
        ( True
        , If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5)
        , Plus (Mult (Num 4, Num 1), Num 1) ))));
    print_endline("17) " ^ string_of_exp (multi_step
    (If
        ( IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1))
        , If
            ( True
            , If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1))
            , Num 5 )
        , Num 5 ))));
    
    print_endline("18) Eval_error");
    (*print_endline("18) " ^ string_of_exp (multi_step
    (If
        ( IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1))))
        , IsZero True
        , Num 1 ))));*)

    print_endline("19) " ^ string_of_exp (multi_step
    (Plus
        ( Num 1
        , Plus
            ( Num (-1)
            , If
                ( IsZero (Plus (Num 1, If (True, Num 1, Num 2)))
                , Plus (Num 1, Num 2)
                , Mult (Num 2, Num 2)))))));
    
    print_endline("20) Eval_error");
    (*print_endline("20) " ^ string_of_exp (multi_step
    (Plus
        ( Num (-1)
        , If
            ( IsZero (Plus (Num 5, Num (-4)))
            , Mult (Num 123, Plus (Num 5, Num (-4)))
            , IsZero (Num 0))))));*)

    print_newline();
    print_endline("== Type System ==");
    print_endline("1) " ^ string_of_typ (type_check True));
    print_endline("2) " ^ string_of_typ (type_check False));
    print_endline("3) " ^ string_of_typ (type_check (Num(0))));
    print_endline("4) " ^ string_of_typ (type_check (IsZero (Num 0))));
    print_endline("5) " ^ string_of_typ (type_check (IsZero (Plus (Num 1,
    Num 1)))));
    print_endline("6) " ^ string_of_typ (type_check (IsZero (Plus (Plus (Num 2,
    Num (-1)), Num 1)))));
    print_endline("7) " ^ string_of_typ (type_check (Plus (Plus (Num (-1), Num
    1), Plus (Num (-1), Num 1)))));
    print_endline("8) " ^ string_of_typ (type_check (Plus (Num (-1), Plus (Mult
    (Num 2, Num 2), Num 1)))));
    print_endline("9) " ^ string_of_typ (type_check (Plus (Plus (Plus (Num 2,
    Num (-1)), Num 1), Num (-1)))));

    print_endline("10) Type_error");
    (*print_endline("10) " ^ string_of_typ (type_check (Plus (IsZero (Plus (Num
    (-1), Num 1)), Num 1))));*)

    print_endline("11) Type_error");
    (*print_endline("11) " ^ string_of_typ (type_check (IsZero (If (IsZero
    (Num 0), True, Num 0)))));*)

    print_endline("12) Type_error");
    (*print_endline("12) " ^ string_of_typ (type_check
   (IsZero
      (If
         ( IsZero (Mult (Num 5, Num 0))
         , If (False, Num 0, IsZero (Plus (Num (-1), Num 0)))
         , Num 0 )))));*)

    print_endline("13) " ^ string_of_typ (type_check (If (IsZero (Plus
    (Num (-1), Num 1)), Num 2, True))));
    print_endline("14) " ^ string_of_typ (type_check
   (If
      ( If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True)
      , Mult (Num 1, Num 2)
      , True ))));
    print_endline("15) " ^ string_of_typ (type_check
   (If
      ( If (IsZero (Mult (Num 0, Num 0)), IsZero (Num 2), Num 0)
      , Mult (Num 2, Mult (Num 1, Num 1))
      , Plus
          ( Plus
              ( Plus
                  ( Plus (If (IsZero (Num 0), Num 1, Num 0), Num (-1))
                  , Num 1 )
              , Num (-1) )
          , Num 1 ) ))));
    print_endline("16) " ^ string_of_typ (type_check
   (If
      ( True
      , If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5)
      , Plus (Mult (Num 4, Num 1), Num 1) ))));
    print_endline("17) " ^ string_of_typ (type_check
   (If
      ( IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1))
      , If
          ( True
          , If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1))
          , Num 5 )
      , Num 5 ))));

    print_endline("18) Type_error");
    (*print_endline("18) " ^ string_of_typ (type_check
   (If
      ( IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1))))
      , IsZero True
      , Num 1 ))));*)

    print_endline("19) " ^ string_of_typ (type_check
   (Plus
      ( Num 1
      , Plus
          ( Num (-1)
          , If
              ( IsZero (Plus (Num 1, If (True, Num 1, Num 2)))
              , Plus (Num 1, Num 2)
              , Mult (Num 2, Num 2) ) ) ))));

    print_endline("20) Type_error");
    (*print_endline("20) " ^ string_of_typ (type_check
   (Plus
      ( Num (-1)
      , If
          ( IsZero (Plus (Num 5, Num (-4)))
          , Mult (Num 123, Plus (Num 5, Num (-4)))
          , IsZero (Num 0) ) ))));*)
