-- Module: Mod3
-- The following is the direct show instance of core : 
[ NonRec $trModule
    ( App
        ( App ( Var Module ) ( Var $trModule_s4vS ) ) ( Var $trModule_s4vU )
    )
, NonRec $trModule_s4vU
    ( App ( Var TrNameS ) ( Var $trModule_s4vT ) )
, NonRec $trModule_s4vT ( Lit Mod3 )
, NonRec $trModule_s4vS
    ( App ( Var TrNameS ) ( Var $trModule_s4vR ) )
, NonRec $trModule_s4vR ( Lit main )
, NonRec dupli
    ( Let
        ( Rec
            [
                ( dupli_R4vY
                , Lam ds_d4vF
                    ( Tick src<NeverExistedLocalFile.hs:
                        ( 5
                        , 1
                        )-
                        ( 6
                        , 32
                        )>
                        ( Case ( Var ds_d4vF ) wild_X1
                            ( TyConApp [] [ TyVarTy a_a4pK ] )
                            [ Alt
                                ( DataAlt [] ) []
                                ( Tick src<NeverExistedLocalFile.hs:5:12-13>
                                    ( Var [] )
                                )
                            , Alt ( DataAlt : )
                                [ x_a3OO
                                , xs_a3OP
                                ]
                                ( Tick src<NeverExistedLocalFile.hs:6:16-32>
                                    ( App
                                        ( App ( Var ++ )
                                            ( Tick src<NeverExistedLocalFile.hs:6:16-20>
                                                ( App ( Var build )
                                                    ( Lam c_d4vJ
                                                        ( Lam n_d4vK
                                                            ( App
                                                                ( App ( Var c_d4vJ )
                                                                    ( Tick src<NeverExistedLocalFile.hs:6:17> ( Var x_a3OO ) )
                                                                )
                                                                ( App
                                                                    ( App ( Var c_d4vJ )
                                                                        ( Tick src<NeverExistedLocalFile.hs:6:19> ( Var x_a3OO ) )
                                                                    ) ( Var n_d4vK )
                                                                )
                                                            )
                                                        )
                                                    )
                                                )
                                            )
                                        )
                                        ( Tick src<NeverExistedLocalFile.hs:6:25-32>
                                            ( App ( Var dupli_R4vY )
                                                ( Tick src<NeverExistedLocalFile.hs:6:31-32> ( Var xs_a3OP ) )
                                            )
                                        )
                                    )
                                )
                            ]
                        )
                    )
                )
            ]
        ) ( Var dupli_R4vY )
    )
]
-- The following is the pretty printed core: 
[$trModule :: Module
 [LclIdX,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
 $trModule = Module $trModule_s4vS $trModule_s4vU,
 $trModule_s4vU :: TrName
 [LclId,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
 $trModule_s4vU = TrNameS $trModule_s4vT,
 $trModule_s4vT :: Addr#
 [LclId,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 20 0}]
 $trModule_s4vT = "Mod3"#,
 $trModule_s4vS :: TrName
 [LclId,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
 $trModule_s4vS = TrNameS $trModule_s4vR,
 $trModule_s4vR :: Addr#
 [LclId,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 20 0}]
 $trModule_s4vR = "main"#,
 dupli [Occ=LoopBreaker] :: forall a. [a] -> [a]
 [LclIdX,
  Arity=1,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [30] 150 10}]
 dupli
   = letrec {
       dupli_R4vY :: forall a. [a] -> [a]
       [LclId]
       dupli_R4vY
         = \ (ds_d4vF :: [a_a4pK]) ->
             src<NeverExistedLocalFile.hs:(5,1)-(6,32)>
             case ds_d4vF of {
               [] -> src<NeverExistedLocalFile.hs:5:12-13> [];
               : x_a3OO xs_a3OP ->
                 src<NeverExistedLocalFile.hs:6:16-32>
                 ++
                   (src<NeverExistedLocalFile.hs:6:16-20>
                    build
                      (\ (c_d4vJ :: a_a4pK -> a_d4vI -> a_d4vI) (n_d4vK :: a_d4vI) ->
                         c_d4vJ
                           (src<NeverExistedLocalFile.hs:6:17> x_a3OO)
                           (c_d4vJ (src<NeverExistedLocalFile.hs:6:19> x_a3OO) n_d4vK)))
                   (src<NeverExistedLocalFile.hs:6:25-32>
                    dupli_R4vY (src<NeverExistedLocalFile.hs:6:31-32> xs_a3OP))
             }; } in
     dupli_R4vY]
