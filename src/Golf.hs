import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Mockup

golf = ModularProgram
    { signatures      = Set.fromList
                            [ (Type "R -> R -> R -> P ()", "Successes")
                            , (Type "R -> R"             , "P")
                            , (Type "R -> P (R)"         , "PAngle")
                            , (Type "P (R)"              , "AngleAndDistance")
                            , (Type "R -> P ()"          , "PDistance")
                            , (Type "P (R, R)"           , "DistanceModel")
                            ]
    , implementations = Set.fromList
        [ ModuleImplementation
            { implBody      = Code { moduleReferences = Set.empty
                                   , moduleInstances  = Set.empty
                                   }
            , implArgs      = ["y", "n", "p"]
            , implSignature = "Successes"
            , implParams    = Set.empty
            , implName      = "Binomial"
            }
        , ModuleImplementation
            { implBody      = Code { moduleReferences = Set.empty
                                   , moduleInstances  = Set.empty
                                   }
            , implArgs      = ["x"]
            , implSignature = "P"
            , implParams    = Set.fromList [Param "a", Param "b"]
            , implName      = "Logit"
            }
        , ModuleImplementation
            { implBody      = Code
                { moduleReferences = Set.empty
                , moduleInstances  = Set.fromList
                                         [("PAngle", Nothing, [Expr "x"])]
                }
            , implArgs      = ["x"]
            , implSignature = "P"
            , implParams    = Set.empty
            , implName      = "Angular"
            }
        , ModuleImplementation
            { implBody      = Code { moduleReferences = Set.empty
                                   , moduleInstances  = Set.empty
                                   }
            , implArgs      = ["x"]
            , implSignature = "PAngle"
            , implParams    = Set.fromList [Param "sigma_angle"]
            , implName      = "PAngle"
            }
        , ModuleImplementation
            { implBody      = Code
                { moduleReferences = Set.empty
                , moduleInstances  = Set.fromList
                                         [ ("PAngle"   , Nothing, [Expr "x"])
                                         , ("PDistance", Nothing, [Expr "x"])
                                         ]
                }
            , implArgs      = ["x"]
            , implSignature = "P"
            , implParams    = Set.empty
            , implName      = "AngleAndDistance"
            }
        , ModuleImplementation
            { implBody      = Code
                { moduleReferences = Set.empty
                , moduleInstances  = Set.fromList
                                         [("DistanceModel", Just "overshot", [])]
                }
            , implArgs      = ["x"]
            , implSignature = "P"
            , implParams    = Set.fromList [Param "sigma_distance"]
            , implName      = "PDistance"
            }
        , ModuleImplementation
            { implBody      = Code { moduleReferences = Set.empty
                                   , moduleInstances  = Set.empty
                                   }
            , implArgs      = ["x"]
            , implSignature = "DistanceModel"
            , implParams    = Set.empty
            , implName      = "Fixed"
            }
        , ModuleImplementation
            { implBody      = Code { moduleReferences = Set.empty
                                   , moduleInstances  = Set.empty
                                   }
            , implArgs      = ["x"]
            , implSignature = "Successes"
            , implParams    = Set.fromList [Param "sigma_y"]
            , implName      = "Proportional"
            }
        , ModuleImplementation
            { implBody      = Code { moduleReferences = Set.empty
                                   , moduleInstances  = Set.empty
                                   }
            , implArgs      = []
            , implSignature = "DistanceModel"
            , implParams    = Set.fromList
                                  [Param "overshot", Param "distance_tolerance"]
            , implName      = "Parametric"
            }
        ]
    , topBody         = Code
                            { moduleReferences = Set.empty
                            , moduleInstances  = Set.fromList
                                                     [ ( "Successes"
                                                       , Nothing
                                                       , [Expr "y", Expr "n", Expr "p"]
                                                       )
                                                     , ("P", Nothing, [Expr "x"])
                                                     ]
                            }
    , topParams       = Set.empty
    }

main = drawModuleTreeGraph golf
