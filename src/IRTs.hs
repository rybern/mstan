{-# LANGUAGE OverloadedStrings #-}
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Mockup
import           ToGraph
import           Types
import           Parsing

irts = ModularProgram
    { signatures      = Set.fromList
                            [ (Type "", "IRT_Prob")
                            , (Type "", "IRT_LogOdds")
                            , (Type "", "Intercept")
                            , (Type "", "Ability")
                            , (Type "", "Difficulty")
                            ]
    , implementations = Set.fromList
        [ ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                    [("IRT_LogOdds", Nothing, [Expr "i", Expr "j"])]
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn = Just . Expr $ "inv_logit(IRT_LogOdds(i, j))"
                    }
                }
            , implArgs      = ["i", "j"]
            , implSignature = "IRT_Prob"
            , implParams    = Set.empty
            , implName      = "without-guesses"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                                        [ ("Intercept" , Nothing, [])
                                        , ("Ability"   , Nothing, [Expr "i"])
                                        , ("Difficulty", Nothing, [Expr "j"])
                                        ]
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn = Just
                                   . Expr
                                   $ "Intercept() + Ability(i) - Difficulty(j)"
                    }
                }
            , implArgs      = ["i", "j"]
            , implSignature = "IRT_LogOdds"
            , implParams    = Set.empty
            , implName      = "no-discrimination"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                                  { moduleInstances = Set.fromList []
                                  , modularCode     = Code
                                                          { codeText   = []
                                                          , codeReturn = Just
                                                                         . Expr
                                                                         $ "alpha"
                                                          }
                                  }
            , implArgs      = []
            , implSignature = "Intercept"
            , implParams    = Set.fromList [Param "alpha"]
            , implName      = "yes"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                                  { moduleInstances = Set.fromList []
                                  , modularCode     = Code
                                                          { codeText   = []
                                                          , codeReturn = Just
                                                                         . Expr
                                                                         $ "0"
                                                          }
                                  }
            , implArgs      = []
            , implSignature = "Intercept"
            , implParams    = Set.empty
            , implName      = "no"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                                  { moduleInstances = Set.fromList []
                                  , modularCode     = Code
                                                          { codeText   = []
                                                          , codeReturn = Just
                                                                         . Expr
                                                                         $ "beta"
                                                          }
                                  }
            , implArgs      = []
            , implSignature = "Ability"
            , implParams    = Set.fromList [Param "beta"]
            , implName      = "unpooled"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                                  { moduleInstances = Set.fromList []
                                  , modularCode     = Code
                                                          { codeText   = []
                                                          , codeReturn = Just
                                                                         . Expr
                                                                         $ "0"
                                                          }
                                  }
            , implArgs      = []
            , implSignature = "Ability"
            , implParams    = Set.empty
            , implName      = "pooled"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                                  { moduleInstances = Set.fromList []
                                  , modularCode     = Code
                                                          { codeText   = []
                                                          , codeReturn = Just
                                                                         . Expr
                                                                         $ "gamma"
                                                          }
                                  }
            , implArgs      = []
            , implSignature = "Difficulty"
            , implParams    = Set.fromList [Param "gamma"]
            , implName      = "unpooled"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                                  { moduleInstances = Set.fromList []
                                  , modularCode     = Code
                                                          { codeText   = []
                                                          , codeReturn = Just
                                                                         . Expr
                                                                         $ "0"
                                                          }
                                  }
            , implArgs      = []
            , implSignature = "Difficulty"
            , implParams    = Set.empty
            , implName      = "pooled"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                                        [ ("Intercept" , Nothing, [])
                                        , ("Ability"   , Nothing, [Expr "i"])
                                        , ("Difficulty", Nothing, [Expr "j"])
                                        ]
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn =
                        Just
                        . Expr
                        $ "Intercept() + delta[j] * Ability(i) - Difficulty(j)"
                    }
                }
            , implArgs      = ["i", "j"]
            , implSignature = "IRT_LogOdds"
            , implParams    = Set.fromList [Param "delta"]
            , implName      = "ability-discrimination"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                                        [ ("Intercept" , Nothing, [])
                                        , ("Ability"   , Nothing, [Expr "i"])
                                        , ("Difficulty", Nothing, [Expr "j"])
                                        ]
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn =
                        Just
                        . Expr
                        $ "delta[j] * (Intercept() + Ability(i) - Difficulty(j))"
                    }
                }
            , implArgs      = ["i", "j"]
            , implSignature = "IRT_LogOdds"
            , implParams    = Set.fromList [Param "delta"]
            , implName      = "total-discrimination"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                    [("IRT_LogOdds", Nothing, [Expr "i", Expr "j"])]
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn =
                        Just
                        . Expr
                        $ "theta[j] + (1 - theta[j]) * inv_logit(IRT_LogOdds(i, j))"
                    }
                }
            , implArgs      = ["i", "j"]
            , implSignature = "IRT_Prob"
            , implParams    = Set.fromList [Param "theta"]
            , implName      = "with-guesses"
            }
        ]
    , topData         = ["int I;", "int J;", "matrix[I, J] y;"]
    , topBody         = ModularCode
        { moduleInstances = Set.fromList
                                [("IRT_Prob", Nothing, [Expr "i", Expr "i"])]
        , modularCode     = Code
                                { codeText   =
                                    [ "for(i in 1:I) {"
                                    , "  for(j in 1:J) {"
                                    , "    y[i, j] ~ bernoulli(IRT_Prob(i, j));"
                                    , "  }"
                                    , "}"
                                    ]
                                , codeReturn = Nothing
                                }
        }
    , topParams       = Set.empty
    }

main = do
    Text.putStrLn $ ""
    mapM_ print $ Map.toList $ implSigs irts
    Text.putStrLn $ ""
    mapM_ print $ Map.toList $ sigImpls irts
    Text.putStrLn $ ""
    Text.putStrLn . showSels $ allSelections irts
    print $ allImplSels (implSigs irts)
                        (sigImpls irts)
                        (ImplNode Nothing "root")
    drawModuleTree irts
    publishGraph "irts-modules" $ moduleTreeGraph irts
    publishGraph "irts-models" $ modelTreeGraph irts

testIRT' :: Map SigName ImplName -> IO ()
testIRT' selections = do
    irts <- readModularProgram "irt.modular.stan"
    printConcreteProgram $ selectModules irts selections

testIRT :: Map SigName ImplName -> IO ()
testIRT = printConcreteProgram . selectModules irts

model1 :: Map SigName ImplName
model1 = Map.fromList
    [ ("Intercept"  , "yes")
    , ("Ability"    , "pooled")
    , ("Difficulty" , "pooled")
    , ("IRT_LogOdds", "no-discrimination")
    , ("IRT_Prob"   , "without-guesses")
    ]

model2A :: Map SigName ImplName
model2A = Map.fromList
    [ ("Intercept"  , "yes")
    , ("Ability"    , "unpooled")
    , ("Difficulty" , "pooled")
    , ("IRT_LogOdds", "no-discrimination")
    , ("IRT_Prob"   , "without-guesses")
    ]

model2B :: Map SigName ImplName
model2B = Map.fromList
    [ ("Intercept"  , "no")
    , ("Ability"    , "unpooled")
    , ("Difficulty" , "pooled")
    , ("IRT_LogOdds", "no-discrimination")
    , ("IRT_Prob"   , "without-guesses")
    ]

model3 :: Map SigName ImplName
model3 = Map.fromList
    [ ("Intercept"  , "no")
    , ("Ability"    , "pooled")
    , ("Difficulty" , "unpooled")
    , ("IRT_LogOdds", "no-discrimination")
    , ("IRT_Prob"   , "without-guesses")
    ]

model4 :: Map SigName ImplName
model4 = Map.fromList
    [ ("Intercept"  , "yes")
    , ("Ability"    , "unpooled")
    , ("Difficulty" , "unpooled")
    , ("IRT_LogOdds", "no-discrimination")
    , ("IRT_Prob"   , "without-guesses")
    ]

model5 :: Map SigName ImplName
model5 = Map.fromList
    [ ("Intercept"  , "yes")
    , ("Ability"    , "unpooled")
    , ("Difficulty" , "pooled")
    , ("IRT_LogOdds", "ability-discrimination")
    , ("IRT_Prob"   , "without-guesses")
    ]

model6 :: Map SigName ImplName
model6 = Map.fromList
    [ ("Intercept"  , "yes")
    , ("Ability"    , "unpooled")
    , ("Difficulty" , "unpooled")
    , ("IRT_LogOdds", "total-discrimination")
    , ("IRT_Prob"   , "without-guesses")
    ]

model7 :: Map SigName ImplName
model7 = Map.fromList
    [ ("Intercept"  , "yes")
    , ("Ability"    , "unpooled")
    , ("Difficulty" , "unpooled")
    , ("IRT_LogOdds", "total-discrimination")
    , ("IRT_Prob"   , "with-guesses")
    ]

readVersion = ModularProgram
    { signatures      = Set.fromList
                            [ (Type "", "Ability")
                            , (Type "", "Difficulty")
                            , (Type "", "IRT_LogOdds")
                            , (Type "", "IRT_Prob")
                            , (Type "", "Intercept")
                            ]
    , implementations = Set.fromList
        [ ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList []
                , modularCode = Code { codeText   = []
                                     , codeReturn = Just (Expr { unExpr = "0" })
                                     }
                }
            , implArgs      = [""]
            , implSignature = "Intercept"
            , implParams    = Set.fromList []
            , implName      = "no"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList []
                , modularCode = Code { codeText   = []
                                     , codeReturn = Just (Expr { unExpr = "0" })
                                     }
                }
            , implArgs      = ["int i"]
            , implSignature = "Ability"
            , implParams    = Set.fromList []
            , implName      = "pooled"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList []
                , modularCode = Code { codeText   = []
                                     , codeReturn = Just (Expr { unExpr = "0" })
                                     }
                }
            , implArgs      = ["int j"]
            , implSignature = "Difficulty"
            , implParams    = Set.fromList []
            , implName      = "pooled"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList []
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn = Just (Expr { unExpr = "alpha" })
                    }
                }
            , implArgs      = [""]
            , implSignature = "Intercept"
            , implParams    = Set.fromList [Param "real alpha"]
            , implName      = "yes"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList []
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn = Just (Expr { unExpr = "beta[i]" })
                    }
                }
            , implArgs      = ["int i"]
            , implSignature = "Ability"
            , implParams    = Set.fromList [Param "vector[I] beta"]
            , implName      = "unpooled"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList []
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn = Just (Expr { unExpr = "gamma[j]" })
                    }
                }
            , implArgs      = ["int j"]
            , implSignature = "Difficulty"
            , implParams    = Set.fromList [Param "vector[J] gamma"]
            , implName      = "unpooled"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                                        [ ( "Ability"
                                          , Nothing
                                          , [Expr { unExpr = "i" }]
                                          )
                                        , ( "Difficulty"
                                          , Nothing
                                          , [Expr { unExpr = "j" }]
                                          )
                                        , ("Intercept", Nothing, [])
                                        ]
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn =
                        Just
                            (Expr
                                { unExpr =
                                    "Intercept() + Ability(i) - Difficulty(j)"
                                }
                            )
                    }
                }
            , implArgs      = ["int i", "int j"]
            , implSignature = "IRT_LogOdds"
            , implParams    = Set.fromList []
            , implName      = "no-discrimination"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                                        [ ( "Ability"
                                          , Nothing
                                          , [Expr { unExpr = "i" }]
                                          )
                                        , ( "Difficulty"
                                          , Nothing
                                          , [Expr { unExpr = "j" }]
                                          )
                                        , ("Intercept", Nothing, [])
                                        ]
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn =
                        Just
                            (Expr
                                { unExpr =
                                    "Intercept() + delta[j] * Ability(i) - Difficulty(j)"
                                }
                            )
                    }
                }
            , implArgs      = ["int i", "int j"]
            , implSignature = "IRT_LogOdds"
            , implParams    = Set.fromList [Param "vector[J] delta"]
            , implName      = "ability-discrimination"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                                        [ ( "Ability"
                                          , Nothing
                                          , [Expr { unExpr = "i" }]
                                          )
                                        , ( "Difficulty"
                                          , Nothing
                                          , [Expr { unExpr = "j" }]
                                          )
                                        , ("Intercept", Nothing, [])
                                        ]
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn =
                        Just
                            (Expr
                                { unExpr =
                                    "delta[j] * (Intercept() + Ability(i) - Difficulty(j))"
                                }
                            )
                    }
                }
            , implArgs      = ["int i", "int j"]
            , implSignature = "IRT_LogOdds"
            , implParams    = Set.fromList [Param "vector[J] delta"]
            , implName      = "total-discrimination"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                    [ ( "IRT_LogOdds"
                      , Nothing
                      , [Expr { unExpr = "i" }, Expr { unExpr = "j" }]
                      )
                    ]
                , modularCode     = Code
                    { codeText   = []
                    , codeReturn = Just
                        (Expr { unExpr = "inv_logit(IRT_LogOdds(i, j))" })
                    }
                }
            , implArgs      = ["int i", "int j"]
            , implSignature = "IRT_Prob"
            , implParams    = Set.fromList []
            , implName      = "without-guesses"
            }
        , ModuleImplementation
            { implBody      = ModularCode
                { moduleInstances = Set.fromList
                    [ ( "IRT_LogOdds"
                      , Nothing
                      , [Expr { unExpr = "i" }, Expr { unExpr = "j" }]
                      )
                    ]
                , modularCode     = Code
                    { codeText   = [ "array[I, J] z = ..;"
                                   , "z[i, j] ~ bernoulli(theta[j]);"
                                   ]
                    , codeReturn = Just
                        (Expr
                            { unExpr =
                                "theta[j] + (1 - theta[j]) * inv_logit(IRT_LogOdds(i, j))"
                            }
                        )
                    }
                }
            , implArgs      = ["int i", "int j"]
            , implSignature = "IRT_Prob"
            , implParams    = Set.fromList [Param "vector[J] theta"]
            , implName      = "with-guesses"
            }
        ]
    , topBody         = ModularCode
        { moduleInstances =
            Set.fromList
                [ ( "IRT_Prob"
                  , Nothing
                  , [Expr { unExpr = "i" }, Expr { unExpr = "j" }]
                  )
                ]
        , modularCode     = Code
                                { codeText   =
                                    ["y[i, j] ~ bernoulli(IRT_Prob(i, j));"]
                                , codeReturn = Nothing
                                }
        }
    , topData = ["int I;", "int J;", "array[I, J] int<lower=0, upper=1> y;"]
    , topParams       = Set.fromList []
    }
