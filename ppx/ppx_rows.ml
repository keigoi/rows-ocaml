open Parsetree

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

let disj_expr ~loc (left_methods : string Location.loc list)
    (right_methods : string Location.loc list) : expression =
  let open Ast_helper in
  let method_ origin (name : string Location.loc) =
    let name = Location.mknoloc name.txt in
    Cf.method_ ~loc name Public @@ Cf.concrete Fresh @@ Exp.send origin name
  in
  let concat_body (l, ls) (r, rs) =
    Exp.object_ ~loc
    @@ Cstr.mk (Pat.any ()) (List.map (method_ l) ls @ List.map (method_ r) rs)
  in
  let method_type (name : string Location.loc) = Of.tag name (Typ.any ()) in
  let split_type names = Typ.object_ ~loc (List.map method_type names) Closed in
  [%expr
    {
      disj_concat =
        (fun l r ->
          [%e concat_body ([%expr l], left_methods) ([%expr r], right_methods)]);
      disj_splitL = (fun lr -> (lr :> [%t split_type left_methods]));
      disj_splitR = (fun lr -> (lr :> [%t split_type right_methods]));
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
  let open Ppxlib in
  Extension.declare "declare_method" Extension.Context.Structure_item
    Ast_pattern.(pstr (many (pstr_eval (pexp_ident (lident __')) nil)))
    (fun ~loc ~path:_ strlocs ->
      [%stri include [%m declare_methods ~loc strlocs]])

let constr =
  Ppxlib.Extension.declare "declare_constr"
    Ppxlib.Extension.Context.Structure_item
    Ppxlib.Ast_pattern.(pstr (many (pstr_eval (pexp_ident (lident __')) nil)))
    (fun ~loc ~path:_ strlocs ->
      [%stri include [%m declare_constrs ~loc strlocs]])

let disj =
  let open Ppxlib in
  Extension.declare "disj" Extension.Context.Expression
    Ast_pattern.(
      pstr
      @@ pstr_eval
           (pexp_tuple
              ((elist (pexp_ident (lident __'))
               ||| map1 ~f:(fun x -> [ x ]) (pexp_ident (lident __')))
              ^:: (elist (pexp_ident (lident __'))
                  ||| map1 ~f:(fun x -> [ x ]) (pexp_ident (lident __')))
              ^:: nil))
           nil
      ^:: nil)
    (fun ~loc ~path:_ ls rs -> disj_expr ~loc ls rs)

let () =
  Ppxlib.Driver.register_transformation ~extensions:[ method_; constr; disj ]
    "ppx_rows"
