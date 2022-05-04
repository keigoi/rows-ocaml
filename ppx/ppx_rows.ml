open Ppxlib
module Location = Ocaml_common.Location

let method_expr ~loc (str0 : string) =
  let strloc = Location.mknoloc str0 in
  Ast_helper.(
    [%expr
      {
        make_obj =
          (fun x ->
            [%e
              Exp.object_ ~loc
              @@ Cstr.mk (Pat.any ())
                   [ Cf.method_ strloc Public (Cf.concrete Fresh [%expr x]) ]]);
        call_obj = (fun obj -> [%e Exp.send ~loc [%expr obj] strloc]);
        method_name = [%e Exp.constant (Const.string strloc.txt)];
      }])

let constr_expr ~loc (str : string) =
  Ast_helper.(
    [%expr
      {
        make_var = (fun x -> [%e Exp.variant ~loc str (Some [%expr x])]);
        match_var =
          (function
          | [%p Pat.variant ~loc str (Some [%pat? x])] -> Some x | _ -> None);
        constr_name = [%e Exp.constant (Const.string str)];
      }])

let disj_expr ~loc (wrap_method : string Location.loc option)
    (left_methods : string Location.loc list)
    (right_methods : string Location.loc list) : expression =
  let open Ast_helper in
  let call_exp e name =
    match wrap_method with
    | Some wrap_method -> Exp.send (Exp.send e wrap_method) name
    | None -> Exp.send e name
  in
  let method_ exp meth =
    Cf.method_ ~loc meth Public @@ Cf.concrete Fresh @@ exp
  in
  let concat_body_method exp (name : string Location.loc) =
    let name = Location.mknoloc name.txt in
    Cf.method_ ~loc name Public @@ Cf.concrete Fresh @@ call_exp exp name
  in
  let concat_body (l, ls) (r, rs) =
    Exp.object_ ~loc
    @@ Cstr.mk (Pat.any ())
         (List.map (concat_body_method l) ls
         @ List.map (concat_body_method r) rs)
  in
  let wrap_exp e =
    match wrap_method with
    | Some wrap_method ->
        Exp.object_ ~loc @@ Cstr.mk (Pat.any ()) [ method_ e wrap_method ]
    | None -> e
  in
  let method_type ?(body = Typ.any ()) (name : string Location.loc) =
    Of.tag name body
  in
  let split_type names = Typ.object_ ~loc (List.map method_type names) Closed in
  let wrap_type t =
    match wrap_method with
    | Some wrap_method ->
        Typ.object_ ~loc [ method_type wrap_method ~body:t ] Closed
    | None -> t
  in
  [%expr
    {
      disj_concat =
        (fun l r ->
          [%e
            wrap_exp
            @@ concat_body ([%expr l], left_methods) ([%expr r], right_methods)]);
      disj_splitL =
        (fun lr -> (lr :> [%t wrap_type @@ split_type left_methods]));
      disj_splitR =
        (fun lr -> (lr :> [%t wrap_type @@ split_type right_methods]));
    }]

let let_ ~loc (strloc : string Location.loc) expr =
  let ident = Ast_helper.Pat.var ~loc:strloc.loc strloc in
  [%stri let [%p ident] = [%e expr]]

let declare_methods ~loc strlocs : Parsetree.module_expr =
  Ast_helper.(
    Mod.structure ~loc
      (List.map
         (fun strloc -> let_ ~loc strloc (method_expr ~loc strloc.txt))
         strlocs))

let declare_constrs ~loc strlocs : Parsetree.module_expr =
  Ast_helper.(
    Mod.structure ~loc
      (List.map
         (fun strloc -> let_ ~loc strloc (constr_expr ~loc strloc.txt))
         strlocs))

let method_ =
  Extension.declare "declare_method" Extension.Context.Structure_item
    Ast_pattern.(pstr (many (pstr_eval (pexp_ident (lident __')) nil)))
    (fun ~loc ~path:_ strlocs ->
      [%stri include [%m declare_methods ~loc strlocs]])

let constr =
  Extension.declare "declare_constr"
    Extension.Context.Structure_item
    Ast_pattern.(pstr (many (pstr_eval (pexp_ident (lident __')) nil)))
    (fun ~loc ~path:_ strlocs ->
      [%stri include [%m declare_constrs ~loc strlocs]])

let ident_or_list () =
  let open Ast_pattern in
  elist (pexp_ident (lident __'))
  ||| map1 ~f:(fun x -> [ x ]) (pexp_ident (lident __'))

let ident_or_list_pair () =
  let open Ast_pattern in
  pexp_tuple (ident_or_list () ^:: ident_or_list () ^:: nil)

let disj =
  Extension.declare "disj" Extension.Context.Expression
    Ast_pattern.(
      pstr
      @@ pstr_eval
           (alt_option
              (pexp_apply
                 (pexp_ident (lident __'))
                 (no_label (ident_or_list_pair ()) ^:: nil))
              (ident_or_list_pair ()))
           nil
      ^:: nil)
    (fun ~loc ~path:_ wrap ls rs -> disj_expr ~loc wrap ls rs)

let () =
  Driver.register_transformation ~extensions:[ method_; constr; disj ]
    "ppx_rows"
