type ('obj, 'mt) method_ = {
  make_obj : 'mt -> 'obj;
  call_obj : 'obj -> 'mt;
  method_name : string;
}

type ('var, 't) constr = {
  make_var : 't -> 'var;
  match_var : 'var -> 't option;
  constr_name : string;
}

type ('lr, 'l, 'r) disj = {
  disj_concat : 'l -> 'r -> 'lr;
  disj_splitL : 'lr -> 'l;
  disj_splitR : 'lr -> 'r;
}

let cast_if_constrs_are_same :
    ('var, 'a) constr -> ('var, 'b) constr -> 'b -> 'a option =
 fun var1 var2 b -> var1.match_var (var2.make_var b)

let cast_obj :
      'obj 'mt1 'mt2.
      ('obj, 'mt1) method_ -> ('obj, 'mt2) method_ -> 'mt2 -> 'mt1 =
 fun obj1 obj2 b -> obj1.call_obj @@ obj2.make_obj b
