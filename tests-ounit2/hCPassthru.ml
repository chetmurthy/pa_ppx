(* camlp5r *)
(* pa_passthru.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Asttools;
open MLast;
open Pa_ppx_base ;
open Ppxutil ;
open Pa_passthru ;

module List = struct
include Stdlib.List ;
value rec map f = fun [
    [][@hashrecons z;] -> [][@hashrecons z;]
  | [a::l][@hashrecons z;] -> let r = f a in ([r :: map f l][@hashrecons z;]) ]
[@@ocaml.warning "-26";] ;
end
;
value option_map f =
  fun
  [ Some x[@hashrecons z;] -> Some (f x)[@hashrecons z;]
  | None[@hashrecons z;] -> None[@hashrecons z;] ]
;

value vala_map f =
    fun
    [ Ploc.VaAnt s [@hashrecons z;] -> Ploc.VaAnt s[@hashrecons z;]
    | Ploc.VaVal x [@hashrecons z;] -> Ploc.VaVal (f x)[@hashrecons z;] ]
;

value class_infos_map arg ~{attributes} f 
  {ciLoc = ciLoc; ciVir = ciVir;
   ciPrm = ciPrm ; ciNam = ciNam;
   ciExp = ciExp; ciAttributes = ciAttributes }[@hashrecons z;] =
  {ciLoc = ciLoc; ciVir = ciVir;
   ciPrm =
     (fun (x1, x2)[@hashrecons z;] -> (x1, x2)[@hashrecons z;]) ciPrm;
   ciNam = ciNam; ciExp = f ciExp; ciAttributes = attributes arg ciAttributes }[@hashrecons z;]
;

value rec ctyp (arg : Ctxt.t)  x =
  match Extfun.apply arg.Ctxt.ef.EF.ctyp x arg ctyp0 with [
    Some x -> x
  | None -> ctyp0 arg x
  | exception Extfun.Failure -> ctyp0 arg x
  ]
and ctyp0 arg =
  let rec self x = ctyp arg x
  and self0 =
    fun
    [ TyAtt loc ct attr[@hashrecons z;] ->
       TyAtt loc (self ct) (attribute arg attr)[@hashrecons z;]
    | TyAcc loc x1 x2[@hashrecons z;] ->
        TyAcc loc (longid arg x1) x2[@hashrecons z;]
    | TyAli loc x1 x2 [@hashrecons z;] →
        TyAli loc (self x1) (self x2)[@hashrecons z;]
    | TyAny loc[@hashrecons z;] →
        TyAny loc[@hashrecons z;]
    | TyApp loc x1 x2[@hashrecons z;] →
        TyApp loc (self x1) (self x2)[@hashrecons z;]
    | TyArr loc x1 x2[@hashrecons z;] →
        TyArr loc (self x1) (self x2)[@hashrecons z;]
    | TyCls loc x1[@hashrecons z;] →
        TyCls loc (vala_map (longid_lident arg) x1)[@hashrecons z;]
    | TyLab loc x1 x2[@hashrecons z;] →
        TyLab loc x1 (self x2)[@hashrecons z;]
    | TyLid loc x1[@hashrecons z;] →
        TyLid loc x1[@hashrecons z;]
    | TyMan loc x1 x2 x3[@hashrecons z;] →
        TyMan loc (self x1) x2 (self x3)[@hashrecons z;]
    | TyObj loc x1 x2[@hashrecons z;] →
        TyObj loc (vala_map (List.map (fun (x1, x2, x3)[@hashrecons z;] → (x1, self x2, attributes arg x3)[@hashrecons z;])) x1) x2[@hashrecons z;]
    | TyOlb loc x1 x2[@hashrecons z;] →
        TyOlb loc x1 (self x2)[@hashrecons z;]
    | TyOpn loc[@hashrecons z;] ->
       TyOpn loc[@hashrecons z;]
    | TyPck loc x1[@hashrecons z;] →
        TyPck loc (module_type arg x1)[@hashrecons z;]
    | TyPol loc x1 x2[@hashrecons z;] →
        TyPol loc x1 (self x2)[@hashrecons z;]
    | TyPot loc x1 x2[@hashrecons z;] →
        TyPot loc x1 (self x2)[@hashrecons z;]
    | TyQuo loc x1[@hashrecons z;] →
        TyQuo loc x1[@hashrecons z;]
    | TyRec loc x1[@hashrecons z;] →
        TyRec loc
          (vala_map
             (List.map (fun (loc, x1, x2, x3, x4) → (loc, x1, x2, self x3, attributes arg x4)))
             x1)[@hashrecons z;]
    | TySum loc x1[@hashrecons z;] →
        TySum loc
          (vala_map
             (List.map
                (generic_constructor arg))
             x1)[@hashrecons z;]
    | TyTup loc x1[@hashrecons z;] →
        TyTup loc (vala_map (List.map self) x1)[@hashrecons z;]
      | TyVrn loc x1 x2[@hashrecons z;] →
        TyVrn loc (vala_map (List.map (poly_variant arg)) x1) x2[@hashrecons z;]
    | TyXtr loc x1 x2[@hashrecons z;] →
        TyXtr loc x1 (option_map (vala_map self) x2)[@hashrecons z;]
    | TyExten loc exten[@hashrecons z;] ->
        TyExten loc (attribute arg exten)[@hashrecons z;]
    ] in
  self0

and generic_constructor arg x =
  match Extfun.apply arg.Ctxt.ef.EF.generic_constructor x arg generic_constructor0 with [
    Some x -> x
  | None -> generic_constructor0 arg x
  | exception Extfun.Failure -> generic_constructor0 arg x
  ]
and generic_constructor0 arg = fun (loc, x1, x2, x3, x4) ->
    (loc, x1, vala_map (List.map (ctyp arg)) x2,
     vala_map (option_map (ctyp arg)) x3, attributes arg x4)
and poly_variant arg =
  fun
  [ PvTag loc x1 x2 x3 x4[@hashrecons z;] →
      PvTag loc x1 x2 (vala_map (List.map (ctyp arg)) x3) (attributes arg x4)[@hashrecons z;]
  | PvInh loc x1[@hashrecons z;] →
      PvInh loc (ctyp arg x1)[@hashrecons z;] ]
and patt arg x =
  match Extfun.apply arg.Ctxt.ef.EF.patt x arg patt0 with [
    Some x -> x
  | None -> patt0 arg x
  | exception Extfun.Failure -> patt0 arg x
  ]
and patt0 arg =
  let rec self x = patt arg x
  and self0 =
    fun
    [ PaAtt loc p attr[@hashrecons z;] ->
       PaAtt loc (self p) (attribute arg attr)[@hashrecons z;]
    | PaPfx loc li p[@hashrecons z;] ->
       PaPfx loc (longid arg li) (self p)[@hashrecons z;]
    | PaLong loc li[@hashrecons z;] ->
       PaLong loc (longid arg li)[@hashrecons z;]
    | PaAli loc x1 x2[@hashrecons z;] →
        PaAli loc (self x1) (self x2)[@hashrecons z;]
    | PaAnt loc x1 → assert False
    | PaAny loc[@hashrecons z;] →
        PaAny loc[@hashrecons z;]
    | PaApp loc x1 x2[@hashrecons z;] →
        PaApp loc (self x1) (self x2)[@hashrecons z;]
    | PaArr loc x1[@hashrecons z;] →
        PaArr loc (vala_map (List.map self) x1)[@hashrecons z;]
    | PaChr loc x1[@hashrecons z;] →
        PaChr loc x1[@hashrecons z;]
    | PaExc loc x1[@hashrecons z;] →
        PaExc loc (self x1)[@hashrecons z;]
    | PaFlo loc x1[@hashrecons z;] →
        PaFlo loc x1[@hashrecons z;]
    | PaInt loc x1 x2[@hashrecons z;] →
        PaInt loc x1 x2[@hashrecons z;]
    | PaLab loc x1[@hashrecons z;] →
        PaLab loc
          (vala_map
             (List.map
                (fun (x1, x2)[@hashrecons z;] → (self x1, vala_map (option_map self) x2)[@hashrecons z;]))
             x1)[@hashrecons z;]
    | PaLaz loc x1[@hashrecons z;] →
        PaLaz loc (self x1)[@hashrecons z;]
    | PaLid loc x1[@hashrecons z;] →
        PaLid loc x1[@hashrecons z;]
    | PaNty loc x1[@hashrecons z;] →
        PaNty loc x1[@hashrecons z;]
    | PaOlb loc x1 x2[@hashrecons z;] →
        PaOlb loc (self x1) (vala_map (option_map (expr arg)) x2)[@hashrecons z;]
    | PaOrp loc x1 x2[@hashrecons z;] →
        PaOrp loc (self x1) (self x2)[@hashrecons z;]
    | PaRec loc x1[@hashrecons z;] →
        PaRec loc (vala_map (List.map (fun (x1, x2)[@hashrecons z;]
                                        → (self x1, self x2)[@hashrecons z;])) x1)[@hashrecons z;]
    | PaRng loc x1 x2[@hashrecons z;] →
        PaRng loc (self x1) (self x2)[@hashrecons z;]
    | PaStr loc x1[@hashrecons z;] →
        PaStr loc x1[@hashrecons z;]
    | PaTup loc x1[@hashrecons z;] →
        PaTup loc (vala_map (List.map self) x1)[@hashrecons z;]
    | PaTyc loc x1 x2 [@hashrecons z;] →
        PaTyc loc (self x1) (ctyp arg x2)[@hashrecons z;]
    | PaTyp loc x1[@hashrecons z;] →
        PaTyp loc (vala_map (longid_lident arg) x1)[@hashrecons z;]
    | PaUnp loc x1 x2[@hashrecons z;] →
        PaUnp loc x1 (option_map (module_type arg) x2)[@hashrecons z;]
    | PaVrn loc x1[@hashrecons z;] →
        PaVrn loc x1[@hashrecons z;]
    | PaXtr loc x1 x2[@hashrecons z;] →
        PaXtr loc x1 (option_map (vala_map self) x2)[@hashrecons z;]
    | PaExten loc exten[@hashrecons z;] ->
        PaExten loc (attribute arg exten)[@hashrecons z;]
    ] in
  self0
and expr arg x =
  match Extfun.apply arg.Ctxt.ef.EF.expr x arg expr0 with [
    Some x -> x
  | None -> expr0 arg x
  | exception Extfun.Failure -> expr0 arg x
  ]
and expr0 arg =
  let rec self x = expr arg x
  and self0 =
    fun
    [ ExAtt loc e attr[@hashrecons z;] ->
       ExAtt loc (self e) (attribute arg attr)[@hashrecons z;]
    | ExAcc loc x1 x2[@hashrecons z;] →
        ExAcc loc (self x1) (self x2)[@hashrecons z;]
    | ExAnt loc x1 → assert False
    | ExApp loc x1 x2[@hashrecons z;] →
        ExApp loc (self x1) (self x2)[@hashrecons z;]
    | ExAre loc x1 x2 x3[@hashrecons z;] →
        ExAre loc x1 (self x2) (vala_map (List.map self) x3)[@hashrecons z;]
    | ExArr loc x1[@hashrecons z;] →
        ExArr loc (vala_map (List.map self) x1)[@hashrecons z;]
    | ExAsr loc x1[@hashrecons z;] →
        ExAsr loc (self x1)[@hashrecons z;]
    | ExAss loc x1 x2[@hashrecons z;] →
        ExAss loc (self x1) (self x2)[@hashrecons z;]
    | ExBae loc x1 x2 x3[@hashrecons z;] →
        ExBae loc x1 (self x2) (vala_map (List.map self) x3)[@hashrecons z;]
    | ExChr loc x1[@hashrecons z;] →
        ExChr loc x1[@hashrecons z;]
    | ExCoe loc x1 x2 x3[@hashrecons z;] →
        ExCoe loc (self x1) (option_map (ctyp arg) x2) (ctyp arg x3)[@hashrecons z;]
    | ExFlo loc x1[@hashrecons z;] →
        ExFlo loc x1[@hashrecons z;]
    | ExFor loc x1 x2 x3 x4 x5[@hashrecons z;] →
        ExFor loc (patt arg x1) (self x2) (self x3) x4 (vala_map (List.map self) x5)[@hashrecons z;]
    | ExFun loc x1[@hashrecons z;] →
        ExFun loc (vala_map (List.map (case_branch arg)) x1)[@hashrecons z;]
    | ExIfe loc x1 x2 x3[@hashrecons z;] →
        ExIfe loc (self x1) (self x2) (self x3)[@hashrecons z;]
    | (ExInt loc x1 x2)[@hashrecons z;] →
        (ExInt loc x1 x2)[@hashrecons z;]
    | ExLab loc x1[@hashrecons z;] →
        ExLab loc
          (vala_map
             (List.map
                (fun (x1, x2) →
                   (patt arg x1, vala_map (option_map self) x2)))
             x1)[@hashrecons z;]
    | ExLaz loc x1[@hashrecons z;] →
        ExLaz loc (self x1)[@hashrecons z;]
    | ExLet loc x1 x2 x3[@hashrecons z;] →
        ExLet loc x1
          (vala_map (List.map (fun (x1, x2, x3) → (patt arg x1, self x2, attributes arg x3))) x2)
          (self x3)[@hashrecons z;]
    | ExLEx loc x1 x2 x3 x4[@hashrecons z;] ->
        ExLEx loc x1 (vala_map (List.map (ctyp arg)) x2) (self x3) (attributes arg x4)[@hashrecons z;]
    | ExLid loc x1[@hashrecons z;] →
        ExLid loc x1[@hashrecons z;]
    | ExLmd loc x1 x2 x3[@hashrecons z;] →
        ExLmd loc x1 (module_expr arg x2) (self x3)[@hashrecons z;]
    | ExLop loc b x1 x2[@hashrecons z;] →
        ExLop loc b (module_expr arg x1) (self x2)[@hashrecons z;]
    | ExMat loc x1 x2[@hashrecons z;] →
        ExMat loc (self x1) (vala_map (List.map (case_branch arg)) x2)[@hashrecons z;]
    | ExNew loc x1[@hashrecons z;] →
        ExNew loc (vala_map (longid_lident arg) x1)[@hashrecons z;]
    | ExObj loc x1 x2[@hashrecons z;] →
        ExObj loc (vala_map (option_map (patt arg)) x1)
          (vala_map (List.map (class_str_item arg)) x2)[@hashrecons z;]
    | ExOlb loc x1 x2[@hashrecons z;] →
        ExOlb loc (patt arg x1) (vala_map (option_map self) x2)[@hashrecons z;]
    | ExOvr loc x1[@hashrecons z;] →
        ExOvr loc (vala_map (List.map (fun (x1, x2) → (x1, self x2))) x1)[@hashrecons z;]
    | ExPck loc x1 x2[@hashrecons z;] →
        ExPck loc (module_expr arg x1)
          (option_map (module_type arg) x2)[@hashrecons z;]
    | ExRec loc x1 x2[@hashrecons z;] →
        ExRec loc
          (vala_map (List.map (fun (x1, x2) → (patt arg x1, self x2))) x1)
          (option_map self x2)[@hashrecons z;]
    | ExSeq loc x1[@hashrecons z;] →
        ExSeq loc (vala_map (List.map self) x1)[@hashrecons z;]
    | ExSnd loc x1 x2[@hashrecons z;] →
        ExSnd loc (self x1) x2[@hashrecons z;]
    | ExSte loc x1 x2 x3[@hashrecons z;] →
        ExSte loc x1 (self x2) (vala_map (List.map self) x3)[@hashrecons z;]
    | ExStr loc x1[@hashrecons z;] →
        ExStr loc x1[@hashrecons z;]
    | ExTry loc x1 x2[@hashrecons z;] →
        ExTry loc (self x1) (vala_map (List.map (case_branch arg)) x2)[@hashrecons z;]
    | ExTup loc x1[@hashrecons z;] →
        ExTup loc (vala_map (List.map self) x1)[@hashrecons z;]
    | ExTyc loc x1 x2[@hashrecons z;] →
        ExTyc loc (self x1) (ctyp arg x2)[@hashrecons z;]
    | ExUid loc x1[@hashrecons z;] →
        ExUid loc x1[@hashrecons z;]
    | ExVrn loc x1[@hashrecons z;] →
        ExVrn loc x1[@hashrecons z;]
    | ExWhi loc x1 x2[@hashrecons z;] →
        ExWhi loc (self x1) (vala_map (List.map self) x2)[@hashrecons z;]
    | ExXtr loc x1 x2[@hashrecons z;] →
        ExXtr loc x1 (option_map (vala_map self) x2)[@hashrecons z;]
    | ExExten loc exten[@hashrecons z;] ->
        ExExten loc exten[@hashrecons z;]
    | ExUnr loc[@hashrecons z;] ->
        ExUnr loc[@hashrecons z;]
    ] in
  self0

and case_branch arg x =
  match Extfun.apply arg.Ctxt.ef.EF.case_branch x arg case_branch0 with [
    Some x -> x
  | None -> case_branch0 arg x
  | exception Extfun.Failure -> case_branch0 arg x
  ]
and case_branch0 arg = fun (x1, x2, x3) →
      (patt arg x1, vala_map (option_map (expr arg)) x2, expr arg x3)

and module_type arg x =
  match Extfun.apply arg.Ctxt.ef.EF.module_type x arg module_type0 with [
    Some x -> x
  | None -> module_type0 arg x
  | exception Extfun.Failure -> module_type0 arg x
  ]
and module_type0 arg =
  let rec self x = module_type arg x
  and self0 =
    fun
    [ MtAtt loc e attr[@hashrecons z;] ->
       MtAtt loc (self e) (attribute arg attr)[@hashrecons z;]
    | MtLong loc x1[@hashrecons z;] →
        MtLong loc (longid arg x1)[@hashrecons z;]
    | MtLongLid loc x1 x2[@hashrecons z;] →
        MtLongLid loc (longid arg x1) x2[@hashrecons z;]
    | MtFun loc arg x3[@hashrecons z;] →
        let arg = vala_map (option_map (fun (idopt, m)[@hashrecons z;] -> (idopt, self m)[@hashrecons z;])) arg in
        MtFun loc arg (self x3)[@hashrecons z;]
    | MtLid loc x1[@hashrecons z;] →
        MtLid loc x1[@hashrecons z;]
    | MtQuo loc x1[@hashrecons z;] →
        MtQuo loc x1[@hashrecons z;]
    | MtSig loc x1[@hashrecons z;] →
        MtSig loc (vala_map (List.map (sig_item arg)) x1)[@hashrecons z;]
    | MtTyo loc x1[@hashrecons z;] →
        MtTyo loc (module_expr arg x1)[@hashrecons z;]
    | MtWit loc x1 x2[@hashrecons z;] →
        MtWit loc (self x1) (vala_map (List.map (with_constr arg)) x2)[@hashrecons z;]
    | MtXtr loc x1 x2[@hashrecons z;] →
        MtXtr loc x1 (option_map (vala_map self) x2)[@hashrecons z;]
    | MtExten loc exten[@hashrecons z;] ->
        MtExten loc (attribute arg exten)[@hashrecons z;]
    ] in
    self0
and sig_item arg x =
  match Extfun.apply arg.Ctxt.ef.EF.sig_item x arg sig_item0 with [
    Some x -> x
  | None -> sig_item0 arg x
  | exception Extfun.Failure -> sig_item0 arg x
  ]
and sig_item0 arg =
  let rec self x = sig_item arg x
  and self0 =
    fun
    [ SgCls loc x1[@hashrecons z;] →
        SgCls loc
          (vala_map (List.map (class_infos_map arg ~{attributes=attributes}  (class_type arg))) x1)[@hashrecons z;]
    | SgClt loc x1[@hashrecons z;] →
        SgClt loc
          (vala_map (List.map (class_infos_map arg ~{attributes=attributes} (class_type arg))) x1)[@hashrecons z;]
    | SgDcl loc x1[@hashrecons z;] →
        SgDcl loc (vala_map (List.map self) x1)[@hashrecons z;]
    | SgDir loc x1 x2[@hashrecons z;] →
        SgDir loc x1 (vala_map (option_map (expr arg)) x2)[@hashrecons z;]
    | SgExc loc x1 x2[@hashrecons z;] →
        SgExc loc (generic_constructor arg x1) (attributes arg x2)[@hashrecons z;]
    | SgExt loc x1 x2 x3 x4[@hashrecons z;] →
        SgExt loc x1 (ctyp arg x2) x3 (attributes arg x4)[@hashrecons z;]
    | SgInc loc x1 x2[@hashrecons z;] →
        SgInc loc (module_type arg x1) (attributes arg x2)[@hashrecons z;]
    | SgMod loc x1 x2[@hashrecons z;] →
        SgMod loc x1
          (vala_map (List.map (fun (x1, x2, x3)[@hashrecons z;] →
                                (x1, module_type arg x2, attributes arg x3)[@hashrecons z;]))
             x2)[@hashrecons z;]
    | SgMty loc x1 x2 x3[@hashrecons z;] →
        SgMty loc x1 (module_type arg x2) (attributes arg x3)[@hashrecons z;]
    | SgMtyAbs loc x1 x2[@hashrecons z;] →
        SgMtyAbs loc x1  (attributes arg x2)[@hashrecons z;]
    | SgMtyAlias loc x1 x2 x3[@hashrecons z;] →
        SgMtyAlias loc x1 (vala_map (longid arg) x2) (attributes arg x3)[@hashrecons z;]
    | SgModSubst loc x1 x2 x3[@hashrecons z;] →
        SgModSubst loc x1 (longid arg x2) (attributes arg x3)[@hashrecons z;]
    | SgOpn loc x1 x2[@hashrecons z;] →
        SgOpn loc (longid arg x1) (attributes arg x2)[@hashrecons z;]
    | SgTyp loc x1 x2[@hashrecons z;] →
        SgTyp loc x1 (vala_map (List.map (type_decl arg)) x2)[@hashrecons z;]
    | SgTypExten loc x1[@hashrecons z;] →
        SgTypExten loc (type_extension arg x1)[@hashrecons z;]
    | SgUse loc x1 x2[@hashrecons z;] →
        SgUse loc x1
          (vala_map (List.map (fun (x1, loc)[@hashrecons z;]
                                → (self x1, loc)[@hashrecons z;])) x2)[@hashrecons z;]
    | SgVal loc x1 x2 x3[@hashrecons z;] →
        SgVal loc x1 (ctyp arg x2) (attributes arg x3)[@hashrecons z;]
    | SgXtr loc x1 x2[@hashrecons z;] →
        SgXtr loc x1 (option_map (vala_map self) x2)[@hashrecons z;]
    | SgFlAtt loc a[@hashrecons z;] ->
        SgFlAtt loc (attribute arg a)[@hashrecons z;]
    | SgExten loc exten attrs[@hashrecons z;] ->
        SgExten loc (attribute arg exten) (attributes arg attrs)[@hashrecons z;]
    ] in
  self0
and with_constr arg x =
  match Extfun.apply arg.Ctxt.ef.EF.with_constr x arg with_constr0 with [
    Some x -> x
  | None -> with_constr0 arg x
  | exception Extfun.Failure -> with_constr0 arg x
  ]
and with_constr0 arg =
  fun
  [ WcMod loc x1 x2[@hashrecons z;] →
      WcMod loc (vala_map (longid arg) x1) (module_expr arg x2)[@hashrecons z;]
  | WcMos loc x1 x2[@hashrecons z;] →
      WcMos loc (vala_map (longid arg) x1) (module_expr arg x2)[@hashrecons z;]
  | WcTyp loc x1 x2 x3 x4[@hashrecons z;] →
      WcTyp loc (vala_map (longid_lident arg) x1) x2 x3 (ctyp arg x4)[@hashrecons z;]
  | WcTys loc x1 x2 x3[@hashrecons z;] →
      WcTys loc (vala_map (longid_lident arg) x1) x2 (ctyp arg x3)[@hashrecons z;] ]
and longid arg x =
  match Extfun.apply arg.Ctxt.ef.EF.longid x arg longid0 with [
    Some x -> x
  | None -> longid0 arg x
  | exception Extfun.Failure -> longid0 arg x
  ]
and longid0 arg =
  let rec self x = longid arg x
  and self0 =
    fun
    [ LiAcc loc x1 x2[@hashrecons z;] →
        LiAcc loc (self x1) x2[@hashrecons z;]
    | LiApp loc x1 x2[@hashrecons z;] →
        LiApp loc (self x1) (self x2)[@hashrecons z;]
    | LiUid loc x1[@hashrecons z;] →
        LiUid loc x1[@hashrecons z;]
    ] in
  self0
and module_expr arg x =
  match Extfun.apply arg.Ctxt.ef.EF.module_expr x arg module_expr0 with [
    Some x -> x
  | None -> module_expr0 arg x
  | exception Extfun.Failure -> module_expr0 arg x
  ]
and module_expr0 arg =
  let rec self x = module_expr arg x
  and self0 =
    fun
    [ MeAtt loc e attr[@hashrecons z;] ->
       MeAtt loc (self e) (attribute arg attr)[@hashrecons z;]
    | MeAcc loc x1 x2[@hashrecons z;] →
        MeAcc loc (self x1) (self x2)[@hashrecons z;]
    | MeApp loc x1 x2[@hashrecons z;] →
        MeApp loc (self x1) (self x2)[@hashrecons z;]
    | MeFun loc farg x3[@hashrecons z;] →
        let farg = vala_map (option_map (fun (idopt, m) -> (idopt, module_type arg m))) farg in
        MeFun loc farg (self x3)[@hashrecons z;]
    | MeStr loc x1[@hashrecons z;] →
        MeStr loc (vala_map (List.map (str_item arg)) x1)[@hashrecons z;]
    | MeTyc loc x1 x2[@hashrecons z;] →
        MeTyc loc (self x1) (module_type arg x2)[@hashrecons z;]
    | MeUid loc x1[@hashrecons z;] →
        MeUid loc x1[@hashrecons z;]
    | MeUnp loc x1 x2 x3[@hashrecons z;] →
        MeUnp loc (expr arg x1) (option_map (module_type arg) x2) (option_map (module_type arg) x3)[@hashrecons z;]
    | MeXtr loc x1 x2[@hashrecons z;] →
        MeXtr loc x1 (option_map (vala_map self) x2)[@hashrecons z;]
    | MeExten loc exten[@hashrecons z;] ->
        MeExten loc (attribute arg exten)[@hashrecons z;]
    ] in
    self0
and str_item arg x =
  match Extfun.apply arg.Ctxt.ef.EF.str_item x arg str_item0 with [
    Some x -> x
  | None -> str_item0 arg x
  | exception Extfun.Failure -> str_item0 arg x
  ]
and str_item0 arg =
  let rec self x = str_item arg x
  and self0 =
    fun
    [ StCls loc x1[@hashrecons z;] →
        StCls loc
          (vala_map (List.map (class_infos_map arg ~{attributes=attributes} (class_expr arg))) x1)[@hashrecons z;]
    | StClt loc x1[@hashrecons z;] →
        StClt loc
          (vala_map (List.map (class_infos_map arg ~{attributes=attributes} (class_type arg))) x1)[@hashrecons z;]
    | StDcl loc x1[@hashrecons z;] →
        StDcl loc (vala_map (List.map self) x1)[@hashrecons z;]
    | StDir loc x1 x2[@hashrecons z;] →
        StDir loc x1 (vala_map (option_map (expr arg)) x2)[@hashrecons z;]
    | StExc loc x1 x2[@hashrecons z;] →
        StExc loc (vala_map (extension_constructor arg) x1) (attributes arg x2)[@hashrecons z;]
    | StExp loc x1 x2[@hashrecons z;] →
        StExp loc (expr arg x1) (attributes arg x2)[@hashrecons z;]
    | StExt loc x1 x2 x3 x4[@hashrecons z;] →
        StExt loc x1 (ctyp arg x2) x3 (attributes arg x4)[@hashrecons z;]
    | StInc loc x1 x2[@hashrecons z;] →
        StInc loc (module_expr arg x1) (attributes arg x2)[@hashrecons z;]
    | StMod loc x1 x2[@hashrecons z;] →
        StMod loc x1
          (vala_map (List.map (fun (x1, x2, x3)[@hashrecons z;] →
           let arg = match uv x1  with [
             Some s -> Ctxt.append_module arg (uv s)
           | None -> arg
           ] in
           (x1, module_expr arg x2, attributes arg x3)[@hashrecons z;]))
             x2)[@hashrecons z;]
    | StMty loc x1 x2 x3[@hashrecons z;] →
        StMty loc x1 (module_type arg x2) (attributes arg x3)[@hashrecons z;]
    | StMtyAbs loc x1 x2[@hashrecons z;] →
        StMtyAbs loc x1 (attributes arg x2)[@hashrecons z;]
    | StOpn loc x1 x2 x3[@hashrecons z;] →
        StOpn loc x1 (module_expr arg x2) (attributes arg x3)[@hashrecons z;]
    | StTyp loc x1 x2[@hashrecons z;] →
        StTyp loc x1 (vala_map (List.map (type_decl arg)) x2)[@hashrecons z;]
    | StTypExten loc x1[@hashrecons z;] →
        StTypExten loc (type_extension arg x1)[@hashrecons z;]
    | StUse loc x1 x2[@hashrecons z;] →
        StUse loc x1
          (vala_map (List.map (fun (x1, loc) → (self x1, loc))) x2)[@hashrecons z;]
    | StVal loc x1 x2[@hashrecons z;] →
        StVal loc x1
          (vala_map
             (List.map (fun (x1, x2, x3)[@hashrecons z;] →
                         (patt arg x1, expr arg x2, attributes arg x3)[@hashrecons z;]))
             x2)[@hashrecons z;]
    | StXtr loc x1 x2[@hashrecons z;] →
        StXtr loc x1 (option_map (vala_map self) x2)[@hashrecons z;]
    | StFlAtt loc a[@hashrecons z;] ->
        StFlAtt loc (attribute arg a)[@hashrecons z;]
    | StExten loc exten attrs[@hashrecons z;] ->
        StExten loc (attribute arg exten) (attributes arg attrs)[@hashrecons z;]
    ] in
  self0
and type_decl arg x =
  match Extfun.apply arg.Ctxt.ef.EF.type_decl x arg type_decl0 with [
    Some x -> x
  | None -> type_decl0 arg x
  | exception Extfun.Failure -> type_decl0 arg x
  ]
and type_decl0 arg
    {tdIsDecl = tdIsDecl ; tdNam = tdNam; tdPrm = tdPrm;
     tdPrv = tdPrv; tdDef = tdDef; tdCon = tdCon;
     tdAttributes = tdAttributes}[@hashrecons z;] =
    {tdIsDecl = tdIsDecl ;
     tdNam = vala_map (fun (loc, x1)[@hashrecons z;] → (loc, x1)[@hashrecons z;]) tdNam; tdPrm = tdPrm;
     tdPrv = tdPrv; tdDef = ctyp arg tdDef;
     tdCon =
       vala_map (List.map (fun (x1, x2)[@hashrecons z;] → (ctyp arg x1, ctyp arg x2)[@hashrecons z;]))
         tdCon;
   tdAttributes = attributes arg tdAttributes}[@hashrecons z;]
and type_extension arg x =
  match Extfun.apply arg.Ctxt.ef.EF.type_extension x arg type_extension0 with [
    Some x -> x
  | None -> type_extension0 arg x
  | exception Extfun.Failure -> type_extension0 arg x
  ]
and type_extension0 arg 
  {teNam = teNam; tePrm = tePrm;
   tePrv = tePrv;
   teECs =  teECs ;
   teAttributes = teAttributes}[@hashrecons z;] =
  {teNam = vala_map (longid_lident arg) teNam; tePrm = tePrm;
   tePrv = tePrv;
   teECs = vala_map (List.map (extension_constructor arg)) teECs ;
   teAttributes = attributes arg teAttributes}[@hashrecons z;]
and extension_constructor arg x =
  match Extfun.apply arg.Ctxt.ef.EF.extension_constructor x arg extension_constructor0 with [
    Some x -> x
  | None -> extension_constructor0 arg x
  | exception Extfun.Failure -> extension_constructor0 arg x
  ]
and extension_constructor0 arg = fun [
    EcTuple loc x1 -> EcTuple loc (generic_constructor arg x1)
  | EcRebind loc x1 x2 x3 -> EcRebind loc x1 x2 (attributes arg x3)
]
and class_type arg x =
  match Extfun.apply arg.Ctxt.ef.EF.class_type x arg class_type0 with [
    Some x -> x
  | None -> class_type0 arg x
  | exception Extfun.Failure -> class_type0 arg x
  ]
and class_type0 arg =
  let rec self x = class_type arg x
  and self0 =
    fun
    [ CtAtt loc e attr[@hashrecons z;] ->
        CtAtt loc (self e) (attribute arg attr)[@hashrecons z;]
    | CtLongLid loc x1 x2[@hashrecons z;] →
        CtLongLid loc (longid arg x1) x2[@hashrecons z;]
    | CtLid loc x1[@hashrecons z;] →
        CtLid loc x1[@hashrecons z;]
    | CtLop loc x1 x2 x3[@hashrecons z;] →
        CtLop loc x1 (longid arg x2) (self x3)[@hashrecons z;]
    | CtCon loc x1 x2[@hashrecons z;] →
        CtCon loc (self x1) (vala_map (List.map (ctyp arg)) x2)[@hashrecons z;]
    | CtFun loc x1 x2[@hashrecons z;] →
        CtFun loc (ctyp arg x1) (self x2)[@hashrecons z;]
    | CtSig loc x1 x2[@hashrecons z;] →
        CtSig loc (vala_map (option_map (ctyp arg)) x1)
          (vala_map (List.map (class_sig_item arg)) x2)[@hashrecons z;]
    | CtXtr loc x1 x2[@hashrecons z;] →
        CtXtr loc x1 (option_map (vala_map self) x2)[@hashrecons z;]
    | CtExten loc exten[@hashrecons z;] ->
        CtExten loc (attribute arg exten)[@hashrecons z;]
    ] in
  self0
and class_sig_item arg x =
  match Extfun.apply arg.Ctxt.ef.EF.class_sig_item x arg class_sig_item0 with [
    Some x -> x
  | None -> class_sig_item0 arg x
  | exception Extfun.Failure -> class_sig_item0 arg x
  ]
and class_sig_item0 arg =
  let rec self x = class_sig_item arg x
  and self0 =
    fun
    [ CgCtr loc x1 x2 x3[@hashrecons z;] →
        CgCtr loc (ctyp arg x1) (ctyp arg x2) (attributes arg x3)[@hashrecons z;]
    | CgDcl loc x1[@hashrecons z;] →
        CgDcl loc (vala_map (List.map self) x1)[@hashrecons z;]
    | CgInh loc x1 x2[@hashrecons z;] →
        CgInh loc (class_type arg x1) (attributes arg x2)[@hashrecons z;]
    | CgMth loc x1 x2 x3 x4[@hashrecons z;] →
        CgMth loc x1 x2 (ctyp arg x3) (attributes arg x4)[@hashrecons z;]
    | CgVal loc x1 x2 x3 x4 x5[@hashrecons z;] →
        CgVal loc x1 x2 x3 (ctyp arg x4) (attributes arg x5)[@hashrecons z;]
    | CgVir loc x1 x2 x3 x4[@hashrecons z;] →
        CgVir loc x1 x2 (ctyp arg x3) (attributes arg x4)[@hashrecons z;]
    | CgFlAtt loc a[@hashrecons z;] ->
        CgFlAtt loc (attribute arg a)[@hashrecons z;]
    | CgExten loc exten[@hashrecons z;] ->
        CgExten loc (attribute arg exten)[@hashrecons z;]
    ] in
  self0
and class_expr arg x =
  match Extfun.apply arg.Ctxt.ef.EF.class_expr x arg class_expr0 with [
    Some x -> x
  | None -> class_expr0 arg x
  | exception Extfun.Failure -> class_expr0 arg x
  ]
and class_expr0 arg =
  let rec self x = class_expr arg x
  and self0 =
    fun
    [ CeAtt loc e attr[@hashrecons z;] ->
       CeAtt loc (self e) (attribute arg attr)[@hashrecons z;]
    | CeApp loc x1 x2[@hashrecons z;] →
        CeApp loc (self x1) (expr arg x2)[@hashrecons z;]
    | CeCon loc x1 x2[@hashrecons z;] →
        CeCon loc (vala_map (longid_lident arg) x1) (vala_map (List.map (ctyp arg)) x2)[@hashrecons z;]
    | CeFun loc x1 x2[@hashrecons z;] →
        CeFun loc (patt arg x1) (self x2)[@hashrecons z;]
    | CeLet loc x1 x2 x3[@hashrecons z;] →
        CeLet loc x1
          (vala_map
             (List.map (fun (x1, x2, x3)[@hashrecons z;] →
                         (patt arg x1, expr arg x2, attributes arg x3)[@hashrecons z;]))
             x2)
          (self x3)[@hashrecons z;]
    | CeLop loc x1 x2 x3[@hashrecons z;] →
        CeLop loc x1 (longid arg x2) (self x3)[@hashrecons z;]
    | CeStr loc x1 x2[@hashrecons z;] →
        CeStr loc (vala_map (option_map (patt arg)) x1)
          (vala_map (List.map (class_str_item arg)) x2)[@hashrecons z;]
    | CeTyc loc x1 x2[@hashrecons z;] →
        CeTyc loc (self x1) (class_type arg x2)[@hashrecons z;]
    | CeXtr loc x1 x2[@hashrecons z;] →
        CeXtr loc x1 (option_map (vala_map self) x2)[@hashrecons z;]
    | CeExten loc exten[@hashrecons z;] ->
        CeExten loc (attribute arg exten)[@hashrecons z;]
    ] in
  self0
and class_str_item arg x =
  match Extfun.apply arg.Ctxt.ef.EF.class_str_item x arg class_str_item0 with [
    Some x -> x
  | None -> class_str_item0 arg x
  | exception Extfun.Failure -> class_str_item0 arg x
  ]
and class_str_item0 arg =
  let rec self x = class_str_item arg x
  and self0 =
    fun
    [ CrCtr loc x1 x2 x3[@hashrecons z;] →
        CrCtr loc (ctyp arg x1) (ctyp arg x2) (attributes arg x3)[@hashrecons z;]
    | CrDcl loc x1[@hashrecons z;] →
        CrDcl loc (vala_map (List.map self) x1)[@hashrecons z;]
    | CrInh loc ovf x1 x2 x3[@hashrecons z;] →
        CrInh loc ovf (class_expr arg x1) x2 (attributes arg x3)[@hashrecons z;]
    | CrIni loc x1 x2 [@hashrecons z;] →
        CrIni loc (expr arg x1) (attributes arg x2)[@hashrecons z;]
    | CrMth loc x1 x2 x3 x4 x5 x6[@hashrecons z;] →
        CrMth loc x1 x2 x3 (vala_map (option_map (ctyp arg)) x4)
          (expr arg x5) (attributes arg x6)[@hashrecons z;]
    | CrVal loc x1 x2 x3 x4 x5[@hashrecons z;] →
        CrVal loc x1 x2 x3 (expr arg x4) (attributes arg x5)[@hashrecons z;]
    | CrVav loc x1 x2 x3 x4[@hashrecons z;] →
        CrVav loc x1 x2 (ctyp arg x3) (attributes arg x4)[@hashrecons z;]
    | CrVir loc x1 x2 x3 x4[@hashrecons z;] →
        CrVir loc x1 x2 (ctyp arg x3) (attributes arg x4)[@hashrecons z;]
    | CrFlAtt loc a [@hashrecons z;] -> 
        CrFlAtt loc (attribute arg a)[@hashrecons z;]
    | CrExten loc exten[@hashrecons z;] -> 
        CrExten loc (attribute arg exten)[@hashrecons z;]
    ] in
  self0
and longid_lident arg (x1, x2) =
    (option_map (vala_map (longid arg)) x1, x2)
and attribute arg (x : attribute) = vala_map (attribute_body arg) x
and attribute_body arg x =
  match Extfun.apply arg.Ctxt.ef.EF.attribute_body x arg attribute_body0 with [
    Some x -> x
  | None -> attribute_body0 arg x
  | exception Extfun.Failure -> attribute_body0 arg x
  ]
and attribute_body0 arg (s, p) =
    let p = match p with [
      StAttr loc x1[@hashrecons z;] ->
      StAttr loc (vala_map (List.map (str_item arg)) x1)[@hashrecons z;]
    | SiAttr loc x1[@hashrecons z;] ->
      SiAttr loc (vala_map (List.map (sig_item arg)) x1)[@hashrecons z;]
    | TyAttr loc x1[@hashrecons z;] ->
      TyAttr loc (vala_map (ctyp arg) x1)[@hashrecons z;]
    | PaAttr loc x1 x2[@hashrecons z;] ->
      PaAttr loc (vala_map (patt arg) x1) (option_map (vala_map (expr arg)) x2)[@hashrecons z;]
    ] in
    (s, p)
and attributes_no_anti arg x1 = List.map (attribute arg) x1
and attributes arg x1 = vala_map (attributes_no_anti arg) x1
and implem arg x =
  match Extfun.apply arg.Ctxt.ef.EF.implem x arg implem0 with [
    Some x -> x
  | None -> implem0 arg x
  | exception Extfun.Failure -> implem0 arg x
  ]
and implem0 arg (l, status) =
    (List.map (fun (si, loc) -> (str_item arg si, loc)) l, status)
and interf arg x =
  match Extfun.apply arg.Ctxt.ef.EF.interf x arg interf0 with [
    Some x -> x
  | None -> interf0 arg x
  | exception Extfun.Failure -> interf0 arg x
  ]
and interf0 arg (l, status) =
    (List.map (fun (si, loc) -> (sig_item arg si, loc)) l, status)
;

value onepass (ctxt,arg) (na, ef) = do {
      Printf.(fprintf stderr "[pass %s]\n%!" na) ;
      let ctxt = Ctxt.{ (ctxt) with ef = ef } in
      (ctxt, implem ctxt arg)
    }
;

value passthru eflist pa_before arg = do {
  let rv = pa_before arg in
  let (l, status) = rv in
  assert (l <> []) ;
  let (_, loc) = List.hd l in
  let ctxt = Ctxt.mk (EF.mk()) loc in
  let (_, rv) = List.fold_left onepass (ctxt,(l,status)) eflist in
  rv
}
;
