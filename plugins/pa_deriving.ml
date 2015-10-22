open Camlp4.PreCast ;;
open Syntax ;;
open Camlp4_to_ppx ;;

let linkme = () ;;

(* copied drop and test_val_longident_dot_lt from
   ${DERIVING}/syntax/common/extend.ml *)

let merge_locs l ls = List.fold_left Token.Loc.merge ls l ;;

let rec drop n l =
  if n <= 0 then
    l
  else
    match l with
    | [] -> []
    | _ :: l -> drop (n - 1) l

let test_val_longident_dot_lt =
  Gram.Entry.of_parser "test_val_longident_dot_lt"
    (fun strm ->
       let rec test_longident_dot pos tokens =
         match tokens with
         | (ANTIQUOT ((""|"id"|"anti"|"list"), _), _) :: tokens ->
           test_longident_dot (pos+1) tokens
         | (UIDENT _, _) :: (KEYWORD ".", _) :: (LIDENT _, _) :: tokens ->
           test_longident_dot (pos+3) tokens
         | _ :: _ ->
           test_delim pos tokens
         | [] -> fetch_more test_longident_dot pos
       and test_delim pos tokens =
         if pos = 0 then
           raise Stream.Failure
         else
           match tokens with
           | (KEYWORD ("<"), _) :: _ -> ()
           | _ :: _ -> raise Stream.Failure
           | [] -> fetch_more test_delim pos
       and fetch_more k pos =
         match drop pos (Stream.npeek (pos + 10) strm) with
         | [] -> raise Stream.Failure
         | tokens -> k pos tokens
       in fetch_more test_longident_dot 0)

let name_conv_uid = function
  | "Json" ->
    Some "json"
  | _ ->
    None

let name_conv_longid uid lid =
  match uid, lid with
  | "Json", "t" ->
    Some "json"
  | "Json", "to_string" ->
    Some "to_json"
  | "Json", "from_string" ->
    Some "of_json"
  | _, _ ->
    None

DELETE_RULE Gram str_item: "type"; type_declaration END ;;

EXTEND Gram
  GLOBAL: str_item expr module_expr;

str_item: [
  [ "type"; type_declaration ->
    <:str_item<>>
  | "type"; type_declaration;
    (loc, l) = [
      "deriving";
      "("; l = LIST0 [x = UIDENT -> x] SEP ",";
      ")" ->
      _loc, l
    ]
    ->
    let s =
      let f uid =
        match name_conv_uid uid with
        | Some plugin ->
          plugin
        | None ->
          failwith
            (uid ^ ": don't know of an equivalent ppx_deriving plug-in")
      in
      List.map f l |> String.concat ", "
    in
    replace loc ("[@@deriving " ^ s ^ "]");
    <:str_item<>>
  ]
];

expr: LEVEL "simple" [[ TRY [
    test_val_longident_dot_lt;
    (lid, loc_lid) = [lid = val_longident -> lid, _loc];
    loc_lbracket   = ["<" -> _loc];
    (y, loc_y)     = [y = ctyp -> y, _loc];
    loc_rbracket   = [">" -> _loc]
    ->
    (match lid with
     | <:ident< $uid:uid$ . $lid:lid$ >> ->
       (match name_conv_longid uid lid with
        | Some s ->
          (let loc = merge_locs [loc_lbracket] loc_lid
           and s = Printf.sprintf "[%%derive.%s: " s in
           replace loc s);
          replace loc_rbracket "]";
          <:expr<>>
        | None ->
          failwith
            (Printf.sprintf
               "%s.%s: don't know of an equivalent ppx_deriving plug-in"
               uid lid))
     | _ ->
       failwith "invalid deriving method application")
  ]]];

module_expr: LEVEL "simple" [[ TRY [
    test_val_longident_dot_lt;
    e1 = val_longident ; "<" ; t = ctyp; ">" ->
    failwith "module derivation not implemented for ppx_deriving"
  ]]];

END
