(** The basis for composing OCaml's structural types in a {i type-safe} manner.
    Skip this section if you don't define a custom role or label, and not
    interested in the typing of global combinators. *)

type ('obj, 'mt) method_ = {
  make_obj : 'mt -> 'obj;
      (** Constructor of an object ['obj] with a method of type ['mt]. *)
  call_obj : 'obj -> 'mt;
      (** Destructor of an object ['obj], calling the method of type ['mt]. *)
  method_name : string;  (** Name of the method (for debug purpose only) *)
}
(** Object's {b method} as a first-class value. Typically, it has the following
    polymorphic type:

    {[ (<lab:'mt>, 'mt) method_ ]}

    which denotes a method [lab]. It can be constructed as follows:

    {[
      {
        make_obj =
          (fun x ->
            object
              method lab = x
            end);
        call_obj = (fun obj -> obj#lab);
      }
    ]} *)

(* constraint 'obj = < .. > *)

(* Polymorphic variant constructor as a first-class value. *)
type ('var, 't) constr = {
  make_var : 't -> 'var;
  match_var : 'var -> 't option;
  constr_name : string;  (** Name of the constructor (for debug purpose only) *)
}
(** An example denoting a constructor [C]:

    {[
      let c : ([> `C of 't ], 't) constr =
        {
          make_var = (fun x -> `C x);
          match_var = (function `C x -> Some x | _ -> None);
        }
    ]} *)

(** {b Disjoint concatenation} of two objects, used in the {!choice} combinator. *)
type ('lr, 'l, 'r) disj = {
  disj_concat : 'l -> 'r -> 'lr;  (** Concatenation of a object *)
  disj_splitL : 'lr -> 'l;
      (** Split the left hand part from the concatenated object. *)
  disj_splitR : 'lr -> 'r;
      (** Split the right hand part from the concatenated object. *)
}
(** For example, the disjoint concatenation [left_or_right] of two object with
    methods [left] and [right] respectively is defined by the following:

    {[
      let left_or_right =
        {
          disj_concat =
            (fun l r ->
              object
                method left = l#left
                method right = r#right
              end);
          disj_splitL = (fun obj -> obj#left);
          disj_splitR = (fun obj -> obj#right);
        }
    ]}

    The above has type [(<left:'t1; right:'t2>, <left:'t1>, <right:'t2>) disj]. *)
(* constraint 'lr = < .. >
   * constraint 'l = < .. >
   * constraint 'r = < .. > *)

val cast_if_constrs_are_same :
  ('var, 'a) constr -> ('var, 'b) constr -> 'b -> 'a option

val cast_obj : ('obj, 'mt1) method_ -> ('obj, 'mt2) method_ -> 'mt2 -> 'mt1
