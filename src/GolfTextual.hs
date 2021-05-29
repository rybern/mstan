{-# LANGUAGE OverloadedStrings #-}
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Mockup
import           ToGraph
import           Types

golf = ModularProgram
    { signatures      = Set.fromList
                            [ (Type "R -> R -> R -> P ()", "Successes")
                            , (Type "R -> R"             , "P")
                            , (Type "R -> P (R)"         , "PAngle")
                            , (Type "R -> P ()"          , "PDistance")
                            , (Type "P (R, R)"           , "DistanceModel")
                            ]
    , implementations = Set.fromList
        [ ModuleImplementation
            { implBody      = ModularCode
                                  { moduleInstances = Set.empty
                                  , modularCode     = Code
                                      { codeText = Text.unlines
                                                         $ ["  y ~ binomial(n, p);"]
                                      , codeReturn = Nothing
                                      }
                                  }
            , implArgs      = ["y", "n", "p"]
            , implSignature = "Successes"
            , implParams    = Set.empty
            , implName      = "Binomial"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.empty
                , modularCode = Code { codeText = ""
                                     , codeReturn = Just (Expr "logit(a + b*x)")
                                     }
                }
            , implArgs      = ["x"]
            , implSignature = "P"
            , implParams    = Set.fromList [Param "a", Param "b"]
            , implName      = "Logit"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                                        [("PAngle", Nothing, [Expr "x"])]
                , modularCode = Code { codeText = Text.unlines $ ["  PAngle(x)"]
                                     , codeReturn = Nothing
                                     }
                }
            , implArgs      = ["x"]
            , implSignature = "P"
            , implParams    = Set.empty
            , implName      = "Angular"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.empty
                , modularCode     = Code
                    { codeText =
                        Text.unlines
                            $ [ "  real r = (1.68 / 2) / 12;"
                              , "  real R = (4.25 / 2) / 12;"
                              , "  vector[J] threshold_angle = asin((R-r) ./ x);"
                              , "  vector[J] p_angle = 2*Phi(threshold_angle / sigma_angle) - 1;"
                              ]
                    , codeReturn = Just (Expr "p_angle")
                    }
                }
            , implArgs      = ["x"]
            , implSignature = "PAngle"
            , implParams    = Set.fromList [Param "sigma_angle"]
            , implName      = "PAngle"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                                        [ ("PAngle"   , Nothing, [Expr "x"])
                                        , ("PDistance", Nothing, [Expr "x"])
                                        ]
                , modularCode     = Code
                    { codeText = Text.unlines $ ["  PAngle(x) + PDistance(x);"]
                    , codeReturn = Nothing
                    }
                }
            , implArgs      = ["x"]
            , implSignature = "P"
            , implParams    = Set.empty
            , implName      = "AngleAndDistance"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                                        [("DistanceModel", Just "overshot", [])]
                , modularCode     = Code
                    { codeText =
                        Text.unlines
                            $ [ "  (real overshot, real distance_tolerance) = DistanceModel();"
                              , "  sigma_distance ~ normal(0, 1);"
                              , "  vector[J] p_distance = Phi((distance_tolerance - overshot)"
                              , "                             ./ ((x + overshot)*sigma_distance))"
                              , "    - Phi((- overshot) ./ ((x + overshot)*sigma_distance));"
                              ]
                    , codeReturn = Just (Expr "p_distance")
                    }
                }
            , implArgs      = ["x"]
            , implSignature = "PDistance"
            , implParams    = Set.fromList [Param "sigma_distance"]
            , implName      = "PDistance"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.empty
                , modularCode     = Code
                    { codeText = Text.unlines
                                       $ [ "  real overshot = 1;"
                                         , "  real distance_tolerance 3;"
                                         ]
                    , codeReturn = Just (Expr "(overshot, distance_tolerance)")
                    }
                }
            , implArgs      = ["x"]
            , implSignature = "DistanceModel"
            , implParams    = Set.empty
            , implName      = "Fixed"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.empty
                , modularCode     = Code
                    { codeText =
                        Text.unlines
                            $ [ "  sigma_y ~ normal(0, 1);"
                              , "  vector[J] raw_proportions = to_vector(y) ./ to_vector(n);"
                              , "  raw_proportions ~ normal(p, sqrt(p .* (1-p) ./ to_vector(n) + sigma_y^2));"
                              ]
                    , codeReturn = Nothing
                    }
                }
            , implArgs      = ["x"]
            , implSignature = "Successes"
            , implParams    = Set.fromList [Param "sigma_y"]
            , implName      = "Proportional"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.empty
                , modularCode     = Code
                    { codeText =
                        Text.unlines
                            $ [ "  overshot ~ normal(1, 5);"
                              , "  distance_tolerance ~ normal(3, 5);"
                              ]
                    , codeReturn = Just (Expr "(overshot, distance_tolerance)")
                    }
                }
            , implArgs      = []
            , implSignature = "DistanceModel"
            , implParams    = Set.fromList
                                  [Param "overshot", Param "distance_tolerance"]
            , implName      = "Parametric"
            }
        ]
    , topBody         = ModularCode
                            { moduleInstances = Set.fromList
                                                    [ ( "Successes"
                                                      , Nothing
                                                      , [Expr "y", Expr "n", Expr "p"]
                                                      )
                                                    , ("P", Nothing, [Expr "x"])
                                                    ]
                            , modularCode     = Code
                                { codeText = Text.unlines $ ["  y ~ Successes(n, P(x));"]
                                , codeReturn = Nothing
                                }
                            }
    , topParams       = Set.empty
    }

main = do
  -- print $ implSigs golf
  -- print $ sigImpls golf
    print $ allSelections golf
    print $ allImplSels (implSigs golf)
                        (sigImpls golf)
                        (ImplNode Nothing "root")
    drawModuleTree golf
    publishGraph "golf-modules" $ moduleTreeGraph golf
    publishGraph "golf-models" $ modelTreeGraph golf

selectionsBinomialLogit :: Map SigName ImplName
selectionsBinomialLogit =
    Map.fromList [("Successes", "Binomial"), ("P", "Logit")]

selectionsAngleAndDistanceBinomial :: Map SigName ImplName
selectionsAngleAndDistanceBinomial = Map.fromList
    [ ("Successes"    , "Binomial")
    , ("P"            , "AngleAndDistance")
    , ("PAngle"       , "PAngle")
    , ("PDistance"    , "PDistance")
    , ("DistanceModel", "Fixed")
    ]
