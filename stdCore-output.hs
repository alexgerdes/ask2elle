-- Module: Test3
-- The following is the direct show instance of core : 
[ NonRec $trModule
    ( App
        ( App ( Var Module ) ( Var $trModule_s3yL ) ) ( Var $trModule_s3yN )
    )
, NonRec $trModule_s3yN
    ( App ( Var TrNameS ) ( Var $trModule_s3yM ) )
, NonRec $trModule_s3yM ( Lit Test3 )
, NonRec $trModule_s3yL
    ( App ( Var TrNameS ) ( Var $trModule_s3yK ) )
, NonRec $trModule_s3yK ( Lit main )
, NonRec dupli
    ( Let
        ( Rec
            [
                ( dupli_R4vW
                , Lam ds_d3yB
                    ( Tick src<NeverExistedLocalFile.hs:
                        ( 5
                        , 1
                        )-
                        ( 6
                        , 28
                        )>
                        ( Case ( Var ds_d3yB ) wild_X1
                            ( TyConApp [] [ TyVarTy a_aQX ] )
                            [ Alt
                                ( DataAlt [] ) []
                                ( Tick src<NeverExistedLocalFile.hs:5:12-13>
                                    ( Var [] )
                                )
                            , Alt ( DataAlt : )
                                [ x_ag1
                                , xs_ag2
                                ]
                                ( Tick src<NeverExistedLocalFile.hs:6:16-28>
                                    ( App
                                        ( App ( Var ++ )
                                            ( Tick src<NeverExistedLocalFile.hs:6:16> ( Var hole_0_H3yJ ) )
                                        )
                                        ( Tick src<NeverExistedLocalFile.hs:6:21-28>
                                            ( App ( Var dupli_R4vW )
                                                ( Tick src<NeverExistedLocalFile.hs:6:27-28> ( Var xs_ag2 ) )
                                            )
                                        )
                                    )
                                )
                            ]
                        )
                    )
                )
            ]
        ) ( Var dupli_R4vW )
    )
]
-- The following is the pretty printed core: 
[$trModule :: Module
 [LclIdX,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
 $trModule = Module $trModule_s3yL $trModule_s3yN,
 $trModule_s3yN :: TrName
 [LclId,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
 $trModule_s3yN = TrNameS $trModule_s3yM,
 $trModule_s3yM :: Addr#
 [LclId,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 30 0}]
 $trModule_s3yM = "Test3"#,
 $trModule_s3yL :: TrName
 [LclId,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 10 10}]
 $trModule_s3yL = TrNameS $trModule_s3yK,
 $trModule_s3yK :: Addr#
 [LclId,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [] 20 0}]
 $trModule_s3yK = "main"#,
 dupli [Occ=LoopBreaker] :: forall a. [a] -> [a]
 [LclIdX,
  Arity=1,
  Unf=Unf{Src=<vanilla>, TopLvl=True, Value=True, ConLike=True,
          WorkFree=True, Expandable=True, Guidance=IF_ARGS [30] 70 10}]
 dupli
   = letrec {
       dupli_R4vW :: forall a. [a] -> [a]
       [LclId]
       dupli_R4vW
         = \ (ds_d3yB :: [a_aQX]) ->
             src<NeverExistedLocalFile.hs:(5,1)-(6,28)>
             case ds_d3yB of {
               [] -> src<NeverExistedLocalFile.hs:5:12-13> [];
               : x_ag1 xs_ag2 ->
                 src<NeverExistedLocalFile.hs:6:16-28>
                 ++
                   (src<NeverExistedLocalFile.hs:6:16> hole_0_H3yJ)
                   (src<NeverExistedLocalFile.hs:6:21-28>
                    dupli_R4vW (src<NeverExistedLocalFile.hs:6:27-28> xs_ag2))
             }; } in
     dupli_R4vW]
