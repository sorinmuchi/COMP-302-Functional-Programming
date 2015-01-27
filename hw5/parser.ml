exception Error

type token = 
  | VAR of (
# 9 "parser.mly"
       (string)
# 8 "parser.ml"
)
  | VAL
  | TRUE
  | TIMES
  | THEN
  | SEMICOLON
  | RPAREN
  | PLUS
  | NUM of (
# 10 "parser.mly"
       (int)
# 20 "parser.ml"
)
  | NEGATE
  | NAME
  | MINUS
  | LPAREN
  | LET
  | LESSTHAN
  | INT
  | IN
  | IF
  | FUN
  | FN
  | FALSE
  | EQUAL
  | EOF
  | END
  | ELSE
  | DARROW
  | COMMA
  | COLON
  | BOOL
  | ARROW

and _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  mutable _menhir_token: token;
  mutable _menhir_startp: Lexing.position;
  mutable _menhir_endp: Lexing.position;
  mutable _menhir_shifted: int
}

and _menhir_state = 
  | MenhirState107
  | MenhirState102
  | MenhirState93
  | MenhirState90
  | MenhirState84
  | MenhirState81
  | MenhirState78
  | MenhirState76
  | MenhirState72
  | MenhirState70
  | MenhirState67
  | MenhirState66
  | MenhirState63
  | MenhirState59
  | MenhirState54
  | MenhirState50
  | MenhirState47
  | MenhirState45
  | MenhirState43
  | MenhirState40
  | MenhirState37
  | MenhirState34
  | MenhirState30
  | MenhirState29
  | MenhirState27
  | MenhirState25
  | MenhirState23
  | MenhirState21
  | MenhirState19
  | MenhirState15
  | MenhirState13
  | MenhirState10
  | MenhirState9
  | MenhirState6
  | MenhirState5
  | MenhirState4
  | MenhirState0


# 1 "parser.mly"
  
open Minml
(* Parser generated via menhir.*)

# 98 "parser.ml"
let _eRR =
  Error

let rec _menhir_goto_nonempty_list_exp_semi_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_exp_semi_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv579) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nonempty_list_exp_semi_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv577) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (s : 'tv_nonempty_list_exp_semi_) = _v in
        ((let _v : 'tv_exps = 
# 31 "parser.mly"
                (s)
# 117 "parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv575) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_exps) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv573 * _menhir_state * 'tv_exps) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv571 * _menhir_state * 'tv_exps) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv567 * _menhir_state * 'tv_exps) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv565 * _menhir_state * 'tv_exps) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
            let _v : (
# 7 "parser.mly"
       (Minml.exp list)
# 141 "parser.ml"
            ) = 
# 28 "parser.mly"
               (s)
# 145 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv563) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 7 "parser.mly"
       (Minml.exp list)
# 153 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv561) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 7 "parser.mly"
       (Minml.exp list)
# 161 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv559) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_1 : (
# 7 "parser.mly"
       (Minml.exp list)
# 169 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv560)) : 'freshtv562)) : 'freshtv564)) : 'freshtv566)) : 'freshtv568)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv569 * _menhir_state * 'tv_exps) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv570)) : 'freshtv572)) : 'freshtv574)) : 'freshtv576)) : 'freshtv578)) : 'freshtv580)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv583 * _menhir_state * 'tv_exp_semi) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nonempty_list_exp_semi_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv581 * _menhir_state * 'tv_exp_semi) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_nonempty_list_exp_semi_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_nonempty_list_exp_semi_ = 
# 126 "/Users/david/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 192 "parser.ml"
         in
        _menhir_goto_nonempty_list_exp_semi_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv582)) : 'freshtv584)
    | _ ->
        _menhir_fail ()

and _menhir_goto_netuple : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_netuple -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv553 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_netuple) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv551 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (t : 'tv_netuple) = _v in
        ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : 'tv_tuple = 
# 99 "parser.mly"
                            (s :: t)
# 214 "parser.ml"
         in
        _menhir_goto_tuple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv552)) : 'freshtv554)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv557 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_netuple) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv555 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (t : 'tv_netuple) = _v in
        ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
        let _v : 'tv_netuple = 
# 103 "parser.mly"
                            (s :: t)
# 230 "parser.ml"
         in
        _menhir_goto_netuple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv556)) : 'freshtv558)
    | _ ->
        _menhir_fail ()

and _menhir_goto_dec : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_dec -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv549 * _menhir_state * 'tv_dec) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv547 * _menhir_state * 'tv_dec) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FUN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | NAME ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | VAL ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | IN ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState84
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState84) : 'freshtv548)) : 'freshtv550)

and _menhir_run47 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv545 * _menhir_state * 'tv_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BOOL ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | INT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | LPAREN ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState47
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv546)

and _menhir_run19 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv543 * _menhir_state * 'tv_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | FN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NEGATE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | NUM _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState19 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19) : 'freshtv544)

and _menhir_run21 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv541 * _menhir_state * 'tv_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | FN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NEGATE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | NUM _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState21
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState21 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState21) : 'freshtv542)

and _menhir_run23 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv539 * _menhir_state * 'tv_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | FN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NEGATE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | NUM _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState23
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState23 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState23) : 'freshtv540)

and _menhir_run25 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv537 * _menhir_state * 'tv_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | FN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NEGATE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | NUM _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState25 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25) : 'freshtv538)

and _menhir_run27 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv535 * _menhir_state * 'tv_exp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | FN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NEGATE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | NUM _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState27
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv536)

and _menhir_goto_tuplename : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_tuplename -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv517 * (
# 9 "parser.mly"
       (string)
# 437 "parser.ml"
        )) * _menhir_state * 'tv_tuplename) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv515 * (
# 9 "parser.mly"
       (string)
# 443 "parser.ml"
        )) * _menhir_state * 'tv_tuplename) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, x), _, t) = _menhir_stack in
        let _v : 'tv_netuplename = 
# 111 "parser.mly"
                              (x :: t)
# 449 "parser.ml"
         in
        _menhir_goto_netuplename _menhir_env _menhir_stack _v) : 'freshtv516)) : 'freshtv518)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv533 * _menhir_state) * _menhir_state * 'tv_tuplename) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv531 * _menhir_state) * _menhir_state * 'tv_tuplename) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv527 * _menhir_state) * _menhir_state * 'tv_tuplename) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv525 * _menhir_state) * _menhir_state * 'tv_tuplename) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv521 * _menhir_state) * _menhir_state * 'tv_tuplename) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv519 * _menhir_state) * _menhir_state * 'tv_tuplename) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | FALSE ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | FN ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | IF ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | LET ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | LPAREN ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | NEGATE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | NUM _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
                | TRUE ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState59
                | VAR _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState59 _v
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState59) : 'freshtv520)) : 'freshtv522)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv523 * _menhir_state) * _menhir_state * 'tv_tuplename) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv524)) : 'freshtv526)) : 'freshtv528)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv529 * _menhir_state) * _menhir_state * 'tv_tuplename) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv530)) : 'freshtv532)) : 'freshtv534)
    | _ ->
        _menhir_fail ()

and _menhir_goto_netuplename : _menhir_env -> 'ttv_tail -> 'tv_netuplename -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv513 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 522 "parser.ml"
    )) = Obj.magic _menhir_stack in
    let (_v : 'tv_netuplename) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv511 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 529 "parser.ml"
    )) = Obj.magic _menhir_stack in
    let (t : 'tv_netuplename) = _v in
    ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
    let _v : 'tv_tuplename = 
# 107 "parser.mly"
                                (x::t)
# 536 "parser.ml"
     in
    _menhir_goto_tuplename _menhir_env _menhir_stack _menhir_s _v) : 'freshtv512)) : 'freshtv514)

and _menhir_goto_args : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_args -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv495 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 549 "parser.ml"
        )) * _menhir_state * 'tv_tp) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv493 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 555 "parser.ml"
        )) * _menhir_state * 'tv_tp) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s), x), _, t), _, a) = _menhir_stack in
        let _v : 'tv_args = 
# 67 "parser.mly"
                                              ((x, Some t) :: a)
# 561 "parser.ml"
         in
        _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv494)) : 'freshtv496)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv499 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 569 "parser.ml"
        )) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv497 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 575 "parser.ml"
        )) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, x), _, a) = _menhir_stack in
        let _v : 'tv_args = 
# 66 "parser.mly"
                   ((x, None) :: a)
# 581 "parser.ml"
         in
        _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v) : 'freshtv498)) : 'freshtv500)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv509 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 589 "parser.ml"
        )) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv507 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 597 "parser.ml"
        )) * _menhir_state * 'tv_args) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv503 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 606 "parser.ml"
            )) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv501 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 613 "parser.ml"
            )) * _menhir_state * 'tv_args) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BOOL ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | INT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | LPAREN ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv502)) : 'freshtv504)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv505 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 634 "parser.ml"
            )) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv506)) : 'freshtv508)) : 'freshtv510)
    | _ ->
        _menhir_fail ()

and _menhir_run34 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv491 * _menhir_state * 'tv_tp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BOOL ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | INT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | LPAREN ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv492)

and _menhir_run37 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_tp -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv489 * _menhir_state * 'tv_tp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BOOL ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | INT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | LPAREN ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState37
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv490)

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_exp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_exp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv315 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 691 "parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv313 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 699 "parser.ml"
        )) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | ELSE | END | FUN | IN | NAME | RPAREN | SEMICOLON | THEN | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv309 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 718 "parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), x), _, s) = _menhir_stack in
            let _v : 'tv_exp = 
# 52 "parser.mly"
                            (Fn (x, None, s))
# 724 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv310)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv311 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 734 "parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)) : 'freshtv314)) : 'freshtv316)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv319 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv317 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, s1), _, s2) = _menhir_stack in
        let _v : 'tv_exp = 
# 49 "parser.mly"
                          (Primop (Times, [s1;s2]))
# 747 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv318)) : 'freshtv320)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv327 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv325 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | ELSE | END | EQUAL | FUN | IN | LESSTHAN | MINUS | NAME | PLUS | RPAREN | SEMICOLON | THEN | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv321 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, s1), _, s2) = _menhir_stack in
            let _v : 'tv_exp = 
# 47 "parser.mly"
                         (Primop (Plus, [s1; s2]))
# 768 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv322)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv323 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)) : 'freshtv326)) : 'freshtv328)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv335 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv333 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | ELSE | END | EQUAL | FUN | IN | LESSTHAN | MINUS | NAME | PLUS | RPAREN | SEMICOLON | THEN | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv329 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, s1), _, s2) = _menhir_stack in
            let _v : 'tv_exp = 
# 48 "parser.mly"
                          (Primop (Minus, [s1; s2]))
# 796 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv330)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv331 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)) : 'freshtv334)) : 'freshtv336)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv343 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv341 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | ELSE | END | EQUAL | FUN | IN | LESSTHAN | NAME | RPAREN | SEMICOLON | THEN | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv337 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, s1), _, s2) = _menhir_stack in
            let _v : 'tv_exp = 
# 46 "parser.mly"
                             (Primop (LessThan, [s1; s2]))
# 828 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv338)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv339 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)) : 'freshtv344)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv351 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv349 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | ELSE | END | EQUAL | FUN | IN | LESSTHAN | NAME | RPAREN | SEMICOLON | THEN | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv345 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, s1), _, s2) = _menhir_stack in
            let _v : 'tv_exp = 
# 45 "parser.mly"
                          (Primop (Equals, [s1;s2]))
# 860 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv347 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)) : 'freshtv350)) : 'freshtv352)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv359 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 875 "parser.ml"
        )) * _menhir_state * 'tv_tp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv357 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 883 "parser.ml"
        )) * _menhir_state * 'tv_tp) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | ELSE | END | FUN | IN | NAME | RPAREN | SEMICOLON | THEN | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv353 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 902 "parser.ml"
            )) * _menhir_state * 'tv_tp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), x), _, t), _, s) = _menhir_stack in
            let _v : 'tv_exp = 
# 51 "parser.mly"
                                         (Fn (x, Some t, s))
# 908 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv354)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv355 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 918 "parser.ml"
            )) * _menhir_state * 'tv_tp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv356)) : 'freshtv358)) : 'freshtv360)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv369 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * _menhir_state) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv363 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv361 * _menhir_state) * _menhir_state * 'tv_exp) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | FN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | IF ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | NEGATE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | NUM _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState43
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState43 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState43) : 'freshtv362)) : 'freshtv364)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv365 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv366)) : 'freshtv368)) : 'freshtv370)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv379 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv377 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv373 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv371 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | FN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | IF ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | NEGATE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | NUM _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState45
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv372)) : 'freshtv374)
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv375 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)) : 'freshtv378)) : 'freshtv380)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv387 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv385 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | ELSE | END | FUN | IN | NAME | RPAREN | SEMICOLON | THEN | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv381 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, s), _, s1), _, s2) = _menhir_stack in
            let _v : 'tv_exp = 
# 44 "parser.mly"
                                         (If (s,s1,s2))
# 1064 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv382)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv383 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)) : 'freshtv386)) : 'freshtv388)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv395 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1079 "parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv393 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1087 "parser.ml"
        )) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | FUN | IN | NAME | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv389 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1108 "parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), x), _, s) = _menhir_stack in
            let _v : 'tv_dec = 
# 74 "parser.mly"
                            (Val (s,x))
# 1114 "parser.ml"
             in
            _menhir_goto_dec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv390)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv391 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1124 "parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv392)) : 'freshtv394)) : 'freshtv396)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv403 * _menhir_state) * _menhir_state * 'tv_tuplename) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv401 * _menhir_state) * _menhir_state * 'tv_tuplename) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | FUN | IN | NAME | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv397 * _menhir_state) * _menhir_state * 'tv_tuplename) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, t), _, s) = _menhir_stack in
            let _v : 'tv_dec = 
# 75 "parser.mly"
                                                (Valtuple (s, t))
# 1156 "parser.ml"
             in
            _menhir_goto_dec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv399 * _menhir_state) * _menhir_state * 'tv_tuplename) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv400)) : 'freshtv402)) : 'freshtv404)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1171 "parser.ml"
        )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv409 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1179 "parser.ml"
        )) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | FUN | IN | NAME | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv405 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1200 "parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), x), _, s) = _menhir_stack in
            let _v : 'tv_dec = 
# 76 "parser.mly"
                             (ByName (s, x))
# 1206 "parser.ml"
             in
            _menhir_goto_dec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv406)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv407 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1216 "parser.ml"
            )) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)) : 'freshtv410)) : 'freshtv412)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv419 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1225 "parser.ml"
        )) * _menhir_state * 'tv_args) * _menhir_state * 'tv_tp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv417 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1233 "parser.ml"
        )) * _menhir_state * 'tv_args) * _menhir_state * 'tv_tp) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | FUN | IN | NAME | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv413 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1254 "parser.ml"
            )) * _menhir_state * 'tv_args) * _menhir_state * 'tv_tp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), x), _, a1), _, t), _, e) = _menhir_stack in
            let _v : 'tv_dec = 
# 78 "parser.mly"
    (let g (n, o) ts = match o with
      | Some t -> Type.Arrow (t, ts)
      | None -> Type.Arrow (Type.TVar (ref None), ts)
     in
     let f1 = List.fold_right g a1 t in
     let f2 = List.fold_right (fun (n, t) e -> Fn (n, t, e)) a1 e in
     Val (Rec (x, Some f1, f2), x))
# 1266 "parser.ml"
             in
            _menhir_goto_dec _menhir_env _menhir_stack _menhir_s _v) : 'freshtv414)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv415 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1276 "parser.ml"
            )) * _menhir_state * 'tv_args) * _menhir_state * 'tv_tp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)) : 'freshtv418)) : 'freshtv420)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv429 * _menhir_state) * _menhir_state * 'tv_decs) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv427 * _menhir_state) * _menhir_state * 'tv_decs) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv423 * _menhir_state) * _menhir_state * 'tv_decs) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv421 * _menhir_state) * _menhir_state * 'tv_decs) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, d), _, s) = _menhir_stack in
            let _v : 'tv_exp = 
# 53 "parser.mly"
                              (Let (d, s))
# 1301 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv422)) : 'freshtv424)
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv425 * _menhir_state) * _menhir_state * 'tv_decs) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)) : 'freshtv428)) : 'freshtv430)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv443 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * _menhir_state) * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv433 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv431 * _menhir_state * 'tv_exp) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | FN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | IF ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | NEGATE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | NUM _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv432)) : 'freshtv434)
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv437 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv435 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, s) = _menhir_stack in
            let _v : 'tv_simpleexp = 
# 61 "parser.mly"
                        (s)
# 1380 "parser.ml"
             in
            _menhir_goto_simpleexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv436)) : 'freshtv438)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv439 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv440)) : 'freshtv442)) : 'freshtv444)
    | MenhirState93 | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv455 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv453 * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv447 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv445 * _menhir_state * 'tv_exp) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | FN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | IF ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | NEGATE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | NUM _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState93
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState93 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState93) : 'freshtv446)) : 'freshtv448)
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv449 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
            let _v : 'tv_netuple = 
# 102 "parser.mly"
          ([s])
# 1450 "parser.ml"
             in
            _menhir_goto_netuple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv450)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv451 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv452)) : 'freshtv454)) : 'freshtv456)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv459 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv457 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, s) = _menhir_stack in
        let _v : 'tv_exp = 
# 50 "parser.mly"
                 (Primop (Negate, [s]))
# 1469 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv458)) : 'freshtv460)
    | MenhirState102 | MenhirState107 | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv487 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv485 * _menhir_state * 'tv_exp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run27 _menhir_env (Obj.magic _menhir_stack)
        | LESSTHAN ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run23 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run21 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv481 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv479 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
            let _v : 'tv_exp_semi = 
# 39 "parser.mly"
                    (s)
# 1501 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv477) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_exp_semi) = _v in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
            match _menhir_s with
            | MenhirState0 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv469 * _menhir_state * 'tv_exp_semi) = Obj.magic _menhir_stack in
                ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv467 * _menhir_state * 'tv_exp_semi) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | EOF ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv463 * _menhir_state * 'tv_exp_semi) = Obj.magic _menhir_stack in
                    ((let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv461 * _menhir_state * 'tv_exp_semi) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, s) = _menhir_stack in
                    let _v : (
# 6 "parser.mly"
       (Minml.exp)
# 1527 "parser.ml"
                    ) = 
# 36 "parser.mly"
                   (s)
# 1531 "parser.ml"
                     in
                    _menhir_goto_parse _menhir_env _menhir_stack _menhir_s _v) : 'freshtv462)) : 'freshtv464)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv465 * _menhir_state * 'tv_exp_semi) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv466)) : 'freshtv468)) : 'freshtv470)
            | MenhirState107 | MenhirState102 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv475 * _menhir_state * 'tv_exp_semi) = Obj.magic _menhir_stack in
                ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                let _tok = _menhir_env._menhir_token in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv473 * _menhir_state * 'tv_exp_semi) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | FALSE ->
                    _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | FN ->
                    _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | IF ->
                    _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | LET ->
                    _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | LPAREN ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | NEGATE ->
                    _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | NUM _v ->
                    _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                | TRUE ->
                    _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState107
                | VAR _v ->
                    _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _v
                | EOF ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv471 * _menhir_state * 'tv_exp_semi) = Obj.magic _menhir_stack in
                    ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
                    let _v : 'tv_nonempty_list_exp_semi_ = 
# 124 "/Users/david/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 1575 "parser.ml"
                     in
                    _menhir_goto_nonempty_list_exp_semi_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv472)
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv474)) : 'freshtv476)
            | _ ->
                _menhir_fail ()) : 'freshtv478)) : 'freshtv480)) : 'freshtv482)
        | TIMES ->
            _menhir_run19 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv483 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv484)) : 'freshtv486)) : 'freshtv488)
    | _ ->
        _menhir_fail ()

and _menhir_goto_decs : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_decs -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv303 * _menhir_state) * _menhir_state * 'tv_decs) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv301 * _menhir_state) * _menhir_state * 'tv_decs) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv297 * _menhir_state) * _menhir_state * 'tv_decs) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv295 * _menhir_state) * _menhir_state * 'tv_decs) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | FN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | IF ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NEGATE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NUM _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv296)) : 'freshtv298)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv299 * _menhir_state) * _menhir_state * 'tv_decs) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)) : 'freshtv302)) : 'freshtv304)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * _menhir_state * 'tv_dec) * _menhir_state * 'tv_decs) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv305 * _menhir_state * 'tv_dec) * _menhir_state * 'tv_decs) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, d), _, ds) = _menhir_stack in
        let _v : 'tv_decs = 
# 71 "parser.mly"
                     (d :: ds)
# 1655 "parser.ml"
         in
        _menhir_goto_decs _menhir_env _menhir_stack _menhir_s _v) : 'freshtv306)) : 'freshtv308)
    | _ ->
        _menhir_fail ()

and _menhir_reduce49 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_tuplename = 
# 106 "parser.mly"
  ([])
# 1666 "parser.ml"
     in
    _menhir_goto_tuplename _menhir_env _menhir_stack _menhir_s _v

and _menhir_run51 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (string)
# 1673 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv293 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1682 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv289 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1691 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv287 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1698 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | VAR _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv283) = Obj.magic _menhir_stack in
            let (_v : (
# 9 "parser.mly"
       (string)
# 1708 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv281 * (
# 9 "parser.mly"
       (string)
# 1716 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv275 * (
# 9 "parser.mly"
       (string)
# 1725 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _tok = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv273 * (
# 9 "parser.mly"
       (string)
# 1732 "parser.ml"
                )) = _menhir_stack in
                let (_tok : token) = _tok in
                ((match _tok with
                | VAR _v ->
                    _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
                | RPAREN ->
                    _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState54
                | _ ->
                    assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                    _menhir_env._menhir_shifted <- (-1);
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54) : 'freshtv274)) : 'freshtv276)
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv277 * (
# 9 "parser.mly"
       (string)
# 1749 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, x) = _menhir_stack in
                let _v : 'tv_netuplename = 
# 110 "parser.mly"
          ([x])
# 1755 "parser.ml"
                 in
                _menhir_goto_netuplename _menhir_env _menhir_stack _v) : 'freshtv278)
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv279 * (
# 9 "parser.mly"
       (string)
# 1765 "parser.ml"
                )) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv280)) : 'freshtv282)) : 'freshtv284)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv285 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1775 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)) : 'freshtv288)) : 'freshtv290)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1786 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv292)) : 'freshtv294)

and _menhir_reduce2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_args = 
# 65 "parser.mly"
  ([])
# 1796 "parser.ml"
     in
    _menhir_goto_args _menhir_env _menhir_stack _menhir_s _v

and _menhir_run67 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (string)
# 1803 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv271 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 1812 "parser.ml"
    )) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAREN ->
        _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | VAR _v ->
        _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState67 _v
    | COLON ->
        _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState67
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState67) : 'freshtv272)

and _menhir_run68 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv269 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | VAR _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv265 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "parser.mly"
       (string)
# 1841 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv263 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1849 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv259 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1858 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv257 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1865 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BOOL ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | INT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | LPAREN ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70) : 'freshtv258)) : 'freshtv260)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv261 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1886 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)) : 'freshtv264)) : 'freshtv266)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv267 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)) : 'freshtv270)

and _menhir_goto_tp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_tp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv205 * _menhir_state) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv203 * _menhir_state) * _menhir_state * 'tv_tp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ARROW ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv199 * _menhir_state) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let _ = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv197 * _menhir_state) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
            let _v : 'tv_tp = 
# 94 "parser.mly"
                       (t)
# 1923 "parser.ml"
             in
            _menhir_goto_tp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv198)) : 'freshtv200)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv201 * _menhir_state) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)) : 'freshtv204)) : 'freshtv206)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv209 * _menhir_state * 'tv_tp) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv207 * _menhir_state * 'tv_tp) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, t1), _, t2) = _menhir_stack in
        let _v : 'tv_tp = 
# 89 "parser.mly"
    (match t2 with
    | Type.Product l -> Type.Product (t1::l)
    | _ -> Type.Product [t1;t2])
# 1946 "parser.ml"
         in
        _menhir_goto_tp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv208)) : 'freshtv210)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv217 * _menhir_state * 'tv_tp) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv215 * _menhir_state * 'tv_tp) * _menhir_state * 'tv_tp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ARROW ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | DARROW | ELSE | END | EQUAL | FUN | IN | LESSTHAN | MINUS | NAME | PLUS | RPAREN | SEMICOLON | THEN | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv211 * _menhir_state * 'tv_tp) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, t1), _, t2) = _menhir_stack in
            let _v : 'tv_tp = 
# 87 "parser.mly"
                        (Type.Arrow(t1, t2))
# 1969 "parser.ml"
             in
            _menhir_goto_tp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv212)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv213 * _menhir_state * 'tv_tp) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv214)) : 'freshtv216)) : 'freshtv218)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv227 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1984 "parser.ml"
        )) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv225 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 1992 "parser.ml"
        )) * _menhir_state * 'tv_tp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ARROW ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | DARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv221 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2003 "parser.ml"
            )) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv219 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2010 "parser.ml"
            )) * _menhir_state * 'tv_tp) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | FN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | IF ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | NEGATE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | NUM _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState40
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState40 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState40) : 'freshtv220)) : 'freshtv222)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv223 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2045 "parser.ml"
            )) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)) : 'freshtv226)) : 'freshtv228)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv235 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv233 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_tp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ARROW ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | COLON | COMMA | ELSE | END | EQUAL | FUN | IN | LESSTHAN | MINUS | NAME | PLUS | RPAREN | SEMICOLON | THEN | VAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv229 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, s), _, t) = _menhir_stack in
            let _v : 'tv_exp = 
# 54 "parser.mly"
                       (Anno (s,t))
# 2069 "parser.ml"
             in
            _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv230)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv231 * _menhir_state * 'tv_exp) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)) : 'freshtv234)) : 'freshtv236)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv245 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2084 "parser.ml"
        )) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv243 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2092 "parser.ml"
        )) * _menhir_state * 'tv_tp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ARROW ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv239 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2103 "parser.ml"
            )) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv237 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2110 "parser.ml"
            )) * _menhir_state * 'tv_tp) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | LPAREN ->
                _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | VAR _v ->
                _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
            | COLON ->
                _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState72
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72) : 'freshtv238)) : 'freshtv240)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv241 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2133 "parser.ml"
            )) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)) : 'freshtv244)) : 'freshtv246)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv255 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2142 "parser.ml"
        )) * _menhir_state * 'tv_args) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        let _tok = _menhir_env._menhir_token in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv253 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2150 "parser.ml"
        )) * _menhir_state * 'tv_args) * _menhir_state * 'tv_tp) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | ARROW ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv249 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2161 "parser.ml"
            )) * _menhir_state * 'tv_args) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv247 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2168 "parser.ml"
            )) * _menhir_state * 'tv_args) * _menhir_state * 'tv_tp) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | FN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | IF ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | NEGATE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | NUM _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv248)) : 'freshtv250)
        | TIMES ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv251 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2203 "parser.ml"
            )) * _menhir_state * 'tv_args) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv252)) : 'freshtv254)) : 'freshtv256)
    | _ ->
        _menhir_fail ()

and _menhir_goto_nonempty_list_simpleexp_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_nonempty_list_simpleexp_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191 * _menhir_state * 'tv_simpleexp) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nonempty_list_simpleexp_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv189 * _menhir_state * 'tv_simpleexp) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let (xs : 'tv_nonempty_list_simpleexp_) = _v in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_nonempty_list_simpleexp_ = 
# 126 "/Users/david/.opam/system/lib/menhir/standard.mly"
    ( x :: xs )
# 2226 "parser.ml"
         in
        _menhir_goto_nonempty_list_simpleexp_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv190)) : 'freshtv192)
    | MenhirState107 | MenhirState102 | MenhirState0 | MenhirState4 | MenhirState93 | MenhirState90 | MenhirState5 | MenhirState81 | MenhirState78 | MenhirState63 | MenhirState59 | MenhirState9 | MenhirState45 | MenhirState43 | MenhirState10 | MenhirState40 | MenhirState27 | MenhirState25 | MenhirState23 | MenhirState21 | MenhirState19 | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv195) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_nonempty_list_simpleexp_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv193) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (es : 'tv_nonempty_list_simpleexp_) = _v in
        ((let _v : 'tv_exp = 
# 43 "parser.mly"
    (List.fold_left (fun x y -> Apply (x, y)) (List.hd es) (List.tl es))
# 2241 "parser.ml"
         in
        _menhir_goto_exp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv194)) : 'freshtv196)
    | _ ->
        _menhir_fail ()

and _menhir_goto_tuple : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_tuple -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv187 * _menhir_state) * _menhir_state * 'tv_tuple) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv185 * _menhir_state) * _menhir_state * 'tv_tuple) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv181 * _menhir_state) * _menhir_state * 'tv_tuple) = Obj.magic _menhir_stack in
        ((let _ = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv179 * _menhir_state) * _menhir_state * 'tv_tuple) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, t) = _menhir_stack in
        let _v : 'tv_simpleexp = 
# 62 "parser.mly"
                          (Tuple t)
# 2268 "parser.ml"
         in
        _menhir_goto_simpleexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv180)) : 'freshtv182)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * _menhir_state) * _menhir_state * 'tv_tuple) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)) : 'freshtv186)) : 'freshtv188)

and _menhir_reduce9 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_decs = 
# 70 "parser.mly"
  ([])
# 2284 "parser.ml"
     in
    _menhir_goto_decs _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv177 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv163 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv161 * _menhir_state) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | VAR _v ->
            _menhir_run51 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
        | RPAREN ->
            _menhir_reduce49 _menhir_env (Obj.magic _menhir_stack) MenhirState50
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv162)) : 'freshtv164)
    | VAR _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv173 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "parser.mly"
       (string)
# 2318 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv171 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2326 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv167 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2335 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv165 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2342 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | FN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | IF ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | NEGATE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | NUM _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState9
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv166)) : 'freshtv168)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv169 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2375 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv170)) : 'freshtv172)) : 'freshtv174)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv175 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)) : 'freshtv178)

and _menhir_run61 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv159 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | VAR _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "parser.mly"
       (string)
# 2401 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv153 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2409 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv149 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2418 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv147 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2425 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | FN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | IF ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NEGATE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | NUM _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState63
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState63 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState63) : 'freshtv148)) : 'freshtv150)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv151 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2458 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)) : 'freshtv154)) : 'freshtv156)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)) : 'freshtv160)

and _menhir_run65 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv145 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | VAR _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "parser.mly"
       (string)
# 2484 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv139 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2492 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | LPAREN ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | VAR _v ->
            _menhir_run67 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
        | COLON ->
            _menhir_reduce2 _menhir_env (Obj.magic _menhir_stack) MenhirState66
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66) : 'freshtv140)) : 'freshtv142)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)) : 'freshtv146)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv137 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | BOOL ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | INT ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | LPAREN ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState30
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30) : 'freshtv138)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv135) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_tp = 
# 93 "parser.mly"
       (Type.Int)
# 2542 "parser.ml"
     in
    _menhir_goto_tp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv136)

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv133) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_tp = 
# 92 "parser.mly"
       (Type.Bool)
# 2555 "parser.ml"
     in
    _menhir_goto_tp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv134)

and _menhir_goto_simpleexp : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simpleexp -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv131 * _menhir_state * 'tv_simpleexp) = Obj.magic _menhir_stack in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv129 * _menhir_state * 'tv_simpleexp) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | NUM _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState15
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState15 _v
    | COLON | COMMA | ELSE | END | EQUAL | FUN | IN | LESSTHAN | MINUS | NAME | PLUS | RPAREN | SEMICOLON | THEN | TIMES | VAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * 'tv_simpleexp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, x) = _menhir_stack in
        let _v : 'tv_nonempty_list_simpleexp_ = 
# 124 "/Users/david/.opam/system/lib/menhir/standard.mly"
    ( [ x ] )
# 2587 "parser.ml"
         in
        _menhir_goto_nonempty_list_simpleexp_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv128)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState15) : 'freshtv130)) : 'freshtv132)

and _menhir_discard : _menhir_env -> token =
  fun _menhir_env ->
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = _menhir_env._menhir_lexer lexbuf in
    _menhir_env._menhir_token <- _tok;
    _menhir_env._menhir_startp <- lexbuf.Lexing.lex_start_p;
    _menhir_env._menhir_endp <- lexbuf.Lexing.lex_curr_p;
    let shifted = Pervasives.(+) _menhir_env._menhir_shifted 1 in
    if Pervasives.(>=) shifted 0 then
      _menhir_env._menhir_shifted <- shifted;
    _tok

and _menhir_goto_parse : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 6 "parser.mly"
       (Minml.exp)
# 2610 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv125) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : (
# 6 "parser.mly"
       (Minml.exp)
# 2619 "parser.ml"
    )) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_1 : (
# 6 "parser.mly"
       (Minml.exp)
# 2627 "parser.ml"
    )) = _v in
    (Obj.magic _1 : 'freshtv124)) : 'freshtv126)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49 * _menhir_state * 'tv_exp_semi) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv50)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv51) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv52)
    | MenhirState93 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv53 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv55 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState84 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv57 * _menhir_state * 'tv_dec) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv59 * _menhir_state) * _menhir_state * 'tv_decs) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv61 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2668 "parser.ml"
        )) * _menhir_state * 'tv_args) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv63 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2677 "parser.ml"
        )) * _menhir_state * 'tv_args) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv65 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2686 "parser.ml"
        )) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv67 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2695 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState67 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69 * _menhir_state * (
# 9 "parser.mly"
       (string)
# 2704 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv71 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2713 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState63 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2722 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState59 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state) * _menhir_state * 'tv_tuplename) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv77 * (
# 9 "parser.mly"
       (string)
# 2736 "parser.ml"
        )) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv78)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv83 * _menhir_state) * _menhir_state * 'tv_exp) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState43 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state) * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState40 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv87 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2764 "parser.ml"
        )) * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv89 * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91 * _menhir_state * 'tv_tp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv95 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2788 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv97 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState25 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv99 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState23 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv101 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState21 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv103 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv105 * _menhir_state * 'tv_exp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState15 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state * 'tv_simpleexp) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState13 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv109 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2827 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState10 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv113 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 2841 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv115 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv122)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 9 "parser.mly"
       (string)
# 2868 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv47) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (s : (
# 9 "parser.mly"
       (string)
# 2878 "parser.ml"
    )) = _v in
    ((let _v : 'tv_simpleexp = 
# 60 "parser.mly"
          (Var s)
# 2883 "parser.ml"
     in
    _menhir_goto_simpleexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv48)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv45) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_simpleexp = 
# 58 "parser.mly"
       (Bool true)
# 2896 "parser.ml"
     in
    _menhir_goto_simpleexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv46)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 10 "parser.mly"
       (int)
# 2903 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv43) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (s : (
# 10 "parser.mly"
       (int)
# 2913 "parser.ml"
    )) = _v in
    ((let _v : 'tv_simpleexp = 
# 57 "parser.mly"
          (Int s)
# 2918 "parser.ml"
     in
    _menhir_goto_simpleexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv44)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv41 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | FN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NEGATE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NUM _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv42)

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv39 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | NEGATE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | NUM _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv37) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState5 in
        ((let _v : 'tv_tuple = 
# 98 "parser.mly"
  ([])
# 2986 "parser.ml"
         in
        _menhir_goto_tuple _menhir_env _menhir_stack _menhir_s _v) : 'freshtv38)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5) : 'freshtv40)

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv35 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FUN ->
        _menhir_run65 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | NAME ->
        _menhir_run61 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | VAL ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | IN ->
        _menhir_reduce9 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv36)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv33 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | FN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NEGATE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | NUM _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState10
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState10 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState10) : 'freshtv34)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _tok = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv31 * _menhir_state) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | VAR _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv27 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 9 "parser.mly"
       (string)
# 3060 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _tok = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv25 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 3068 "parser.ml"
        )) = _menhir_stack in
        let (_tok : token) = _tok in
        ((match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv17 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 3077 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv15 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 3084 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | BOOL ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INT ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LPAREN ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv16)) : 'freshtv18)
        | DARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv21 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 3103 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _tok = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv19 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 3110 "parser.ml"
            )) = _menhir_stack in
            let (_tok : token) = _tok in
            ((match _tok with
            | FALSE ->
                _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | FN ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | IF ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | LET ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | NEGATE ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | NUM _v ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
            | TRUE ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState13
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState13 _v
            | _ ->
                assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
                _menhir_env._menhir_shifted <- (-1);
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState13) : 'freshtv20)) : 'freshtv22)
        | _ ->
            assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
            _menhir_env._menhir_shifted <- (-1);
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv23 * _menhir_state) * (
# 9 "parser.mly"
       (string)
# 3143 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)) : 'freshtv26)) : 'freshtv28)
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv29 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)) : 'freshtv32)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _ = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _v : 'tv_simpleexp = 
# 59 "parser.mly"
        (Bool false)
# 3164 "parser.ml"
     in
    _menhir_goto_simpleexp _menhir_env _menhir_stack _menhir_s _v) : 'freshtv14)

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_startp = lexbuf.Lexing.lex_start_p;
      _menhir_endp = lexbuf.Lexing.lex_curr_p;
      _menhir_shifted = max_int;
      }

and parse : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 6 "parser.mly"
       (Minml.exp)
# 3183 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv11) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv9) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | EOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState0 in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        ((let _v : (
# 6 "parser.mly"
       (Minml.exp)
# 3205 "parser.ml"
        ) = 
# 35 "parser.mly"
      (raise End_of_file)
# 3209 "parser.ml"
         in
        _menhir_goto_parse _menhir_env _menhir_stack _menhir_s _v) : 'freshtv6)) : 'freshtv8)
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NEGATE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NUM _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv10)) : 'freshtv12))

and parse_exps : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 7 "parser.mly"
       (Minml.exp list)
# 3238 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = () in
    ((assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
    let _tok = _menhir_env._menhir_token in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = _menhir_stack in
    let (_tok : token) = _tok in
    ((match _tok with
    | FALSE ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | FN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | IF ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LET ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NEGATE ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | NUM _v ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | TRUE ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState102
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _v
    | _ ->
        assert (Pervasives.(<>) _menhir_env._menhir_shifted (-1));
        _menhir_env._menhir_shifted <- (-1);
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv2)) : 'freshtv4))



