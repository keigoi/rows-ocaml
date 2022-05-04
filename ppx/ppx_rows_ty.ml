module Typeful = struct
  let pending : (Warnings.loc * string) option ref = ref None

  let fillup_hole self (super : Untypeast.mapper) attr
      (texp : Typedtree.expression) =
    match attr with
    | { Parsetree.attr_name = { txt = "HOLE"; _ }; attr_loc = loc; _ } -> (
        pending := None;
        try Concat.fill_hole ~loc texp
        with Concat.Pending (loc, msg) ->
          pending := Some (loc, msg);
          super.expr self texp)
    | _ -> super.expr self texp

  let untyper =
    let super = Untypeast.default_mapper in
    {
      Untypeast.default_mapper with
      expr =
        (fun self (texp : Typedtree.expression) ->
          match (texp.exp_attributes, texp.exp_extra) with
          | attr :: _, _ -> fillup_hole self super attr texp
          | _, (_, _, attr :: _) :: _ -> fillup_hole super self attr texp
          | _ -> super.expr self texp);
    }

  let rec loop_typer_untyper str =
    try
      Compmisc.init_path ();
      let env = Compmisc.initial_env () in
      let tstr = Compatibility.type_structure env str in
      let untypstr = untyper.structure untyper tstr in
      if str = untypstr then
        match !pending with
        | None -> untypstr
        | Some (loc, msg) -> Location.raise_errorf ~loc "%s" msg
      else loop_typer_untyper untypstr
    with Typecore.Error _ -> str
end

module Typeless = struct
  open Ppxlib

  let make_hole =
    let cnt = ref 0 in
    fun ~loc ->
      cnt := !cnt + 1;
      let open Parsetree in
      let typ = Ast_helper.Typ.var @@ "mpst_tmp_" ^ string_of_int !cnt in
      let exp = [%expr (assert false : [%t typ])] in
      {
        exp with
        pexp_attributes =
          [
            {
              attr_name = { txt = "HOLE"; loc = Location.none };
              attr_loc = loc;
              attr_payload = PStr [];
            };
          ];
      }

  let replace_hashhash str =
    let obj =
      object (this)
        inherit Ast_traverse.map as super

        method! expression exp =
          match exp.pexp_desc with
          | Pexp_apply
              ( {
                  pexp_desc = Pexp_ident { txt = Lident "##"; _ };
                  pexp_loc = loc_hole;
                  _;
                },
                [ (_, arg1); (_, arg2) ] ) ->
              let loc = loc_hole in
              Ast_helper.Exp.apply ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes
                (this#expression arg1)
                [ (Nolabel, make_hole ~loc); (Nolabel, this#expression arg2) ]
          | _ -> super#expression exp
      end
    in
    obj#structure str
end

let transform (str : Ppxlib.Parsetree.structure) =
  Ppxlib.Selected_ast.Of_ocaml.copy_structure
  @@ Typeful.loop_typer_untyper
  @@ Ppxlib.Selected_ast.To_ocaml.copy_structure
  @@ Typeless.replace_hashhash str

let hole =
  Ppxlib.Extension.declare "HOLE" Ppxlib.Extension.Context.expression
    Ppxlib.Ast_pattern.(pstr nil)
    (fun ~loc ~path:_ -> Typeless.make_hole ~loc)

let () =
  Ppxlib.Driver.register_transformation ~extensions:[ hole ]
    ~instrument:(Ppxlib.Driver.Instrument.make ~position:After transform)
    "ppx_rows_ty"
