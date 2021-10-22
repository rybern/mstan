{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import           Data.Attoparsec.Text
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text

import           Types
import           Prelude                 hiding ( takeWhile )
import Indent

parseSelections :: Text -> Maybe (Map SigName ImplName)
parseSelections s = case parseOnly parserSelections s of
    Left  _   -> Nothing
    Right res -> Just res

parserSelections :: Parser (Map SigName ImplName)
parserSelections = do
    let pair = do
            sig  <- takeTill (inClass ",:")
            _    <- char ':'
            impl <- takeTill (inClass ",:")
            return (SigName sig, ImplName impl)
    pairs <- sepBy pair ","
    return $ Map.fromList pairs

parseSigLine :: FullSigName -> Text -> Maybe ([Expr], Expr -> Text)
parseSigLine sigName line = case parseOnly (parserSigLine sigName) line of
    Left  _   -> Nothing
    Right res -> Just res

parserSigLine :: FullSigName -> Parser ([Expr], Expr -> Text)
parserSigLine (FullSigName sigName) = do
    left  <- manyTill anyChar $ string sigName
    args  <- parserArgs
    right <- many' anyChar
    return (args, \(Expr x) -> Text.pack left <> x <> Text.pack right)

parserArgs :: Parser [Expr]
parserArgs = do
    _    <- char '('
    -- args <- sepBy "," (many' (notChar ')'))
    -- args <- (many' (notChar ')'))
    args <- sepBy (parserTilClosed 0) (choice [",", "|"])
    _    <- char ')'
    -- return . map Expr $ [args]
    return . map Expr . filter (/= "") . map Text.strip $ args

parserTilClosed :: Int -> Parser Text
parserTilClosed open = do
    arg <- takeWhile
        (\c -> c /= ')' && c /= '(' && (open > 0 || (c /= ',' && c /= '|')))
    peek <- peekChar
    if (open == 0 && (peek == Just ')' || peek == Just ',' || peek == Just '|'))
        then return arg
        else choice
            [ char ')' >> (((arg <> ")") <>) <$> parserTilClosed (open - 1))
            , char '(' >> (((arg <> "(") <>) <$> parserTilClosed (open + 1))
            ]

tests :: IO ()
tests = mapM_
    (\(sig, code) -> do
        let Just (args, f) = parseSigLine (FullSigName sig) code
        putStrLn $ "----------"
        putStrLn $ "code \"" ++ Text.unpack code ++ "\""
        putStrLn $ "sig \"" ++ Text.unpack sig ++ "\""
        putStrLn $ "found args \"" ++ show args ++ "\""
        putStrLn $ "found replace \"" ++ show (f (Expr "TEST")) ++ "\""
    )
    [ ("Test"       , "int x = Test()")
    , ("Test"       , "int x = Test(1, x, 5)")
    , ("Test"       , "int x = Test((1 + x), (m + (1 + x))) + 1")
    , ("IRT_LogOdds", "inv_logit(IRT_LogOdds(i, j))")
    , ( "IRT_LogOdds"
      , Text.unlines
          ["", "\n\n y[i, j] ~ bernoulli(inv_logit(IRT_LogOdds(i, j)));\n"]
      )
    ]

parserBlock :: Parser header -> Parser body -> Parser (header, body)
parserBlock parserHeader parserBody = do
    header <- parserHeader
    skipSpace
    _ <- char '{'
    skipSpace
    body <- parserBody
    skipSpace
    _ <- char '}'
    skipSpace
    return (header, body)

parserBraced :: Parser Text
parserBraced = do
    line <- takeWhile (notInClass "{}")
    _    <- char '{'
    ((line <> "{") <>) <$> parserTilClosed' 1

parserTilClosed' :: Int -> Parser Text
parserTilClosed' 0    = return ""
parserTilClosed' open = do
    block <- takeWhile (notInClass "{}")
    -- peek  <- peekChar
    -- return $ Text.pack $ (maybe [] (\p -> [p]) peek)
    -- let open' = traceShow open . traceShow block . traceShow peek $ open
    choice
        [ char '}' >> (((block <> "}") <>) <$> parserTilClosed' (open - 1))
        , char '{' >> (((block <> "{") <>) <$> parserTilClosed' (open + 1))
        ]

parserCode :: Int -> Parser Code
parserCode expectedIndent = do
    ignore
    let matchBody = do
            line <- takeTill (inClass "{};")
            char ';'
            ignore
            (lines, ret) <- matchLines
            return $ (line <> ";" : lines, ret)
        matchReturn = do
            ignore
            string "return "
            expr <- takeTill (inClass "{};")
            char ';'
            ignore
            return ([], Just (Expr expr))
        matchBraced = do
            ignore
            braced <- parserBraced
            ignore
            (lines, ret) <- matchLines
            return (braced : lines, ret)
-- matchBraced = do
--     line <- takeTill (inClass "};")
--     ignore
--     char '{'
--     (lines', ret') <- matchLines
--     char '}'
--     (lines, ret) <- matchLines
--     return (line <> "{" : lines' ++ ["}"] ++ lines, ret)
        matchLines = option ([], Nothing)
            $ choice [matchReturn, matchBody, matchBraced]
    (lines, codeReturn) <- matchLines
    return $ Code { codeText = unindentNestedLines expectedIndent lines, codeReturn = codeReturn }

parserGQ :: Parser Code
parserGQ = do
    ignore
    (_, gq) <- parserBlock (string "generated quantities") (parserCode 2)
    return gq

parserFunctions :: Parser Code
parserFunctions = do
    ignore
    (_, functions) <- parserBlock (string "functions") (parserCode 2)
    return functions

parserTD :: Parser Code
parserTD = do
    ignore
    (_, td) <- parserBlock (string "transformed data") (parserCode 2)
    return td

parserParams :: Parser (Set Param)
parserParams = do
    ignore
    (_, params) <- parserBlock (string "parameters") $ do
        params <- many' $ takeTill (inClass "};") <* char ';' <* ignore
        return . Set.fromList . map (Param . Text.strip) $ params
    return params

parserModule :: Parser (ModuleImplementation Code)
parserModule = choice [parserAssociatedModule, parserSingletonModule]

fieldHead :: Parser (FieldName, [Text])
fieldHead = do
    sigName  <- takeTill (inClass "( {")
    fieldArgs <- moduleArgs
    return (FieldName $ Text.strip sigName, fieldArgs)

parserField :: Parser (ModuleField Code)
parserField = do
  ((justFieldSignature, fieldArgs), fieldBody) <- parserBlock fieldHead (parserCode 2)
  let fieldSignature = Just justFieldSignature
  return $ ModuleField { .. }

parserFields :: Parser [ModuleField Code]
parserFields = many1' parserField

parserAssociatedModule :: Parser (ModuleImplementation Code)
parserAssociatedModule = do
    ((implName, implSignature, ()), (implFunctions, implParams, implTD, implGQ, implFields)) <-
        parserBlock (moduleHead (return ())) (moduleBody parserFields)
    return $ ModuleImplementation { .. }

parserSingletonModule :: Parser (ModuleImplementation Code)
parserSingletonModule = do
    ((implName, implSignature, implArgs), (implFunctions, implParams, implTD, implGQ, implBody)) <-
        parserBlock (moduleHead moduleArgs) (moduleBody (parserCode 1))
    let implFields =
            [ ModuleField { fieldBody      = implBody
                          , fieldSignature = Nothing
                          , fieldArgs      = implArgs
                          }
            ]
    return $ ModuleImplementation { .. }

moduleArgs :: Parser [Text]
moduleArgs = do
    skipSpace
    char '('
    implArgs <- sepBy (takeTill (inClass "|),")) (choice [",", "|"])
    char ')'
    return $ map Text.strip implArgs

moduleHead :: Parser args -> Parser (ImplName, SigName, args)
moduleHead parseArgs = do
    string "module"
    skipSpace
    char '"'
    implName <- takeTill (== '"')
    char '"'
    skipSpace
    sigName  <- takeTill (inClass "( {")
    implArgs <- parseArgs
    return (ImplName implName, SigName $ Text.strip sigName, implArgs)

moduleBody
    :: Parser body
    -> Parser (Maybe Code, Set Param, Maybe Code, Maybe Code, body)
moduleBody parseBody = do
    implFunctions <- option Nothing (Just <$> parserFunctions)
    ignore
    implTD <- option Nothing (Just <$> parserTD)
    ignore
    implParams <- option Set.empty parserParams
    ignore
    implGQ <- option Nothing (Just <$> parserGQ)
    ignore
    implBody <- parseBody
    return (implFunctions, implParams, implTD, implGQ, implBody)

findModules :: Set (SigName, Maybe FieldName) -> Code -> ModularCode
findModules signatures code = ModularCode
    { modularCode     = code
    , moduleInstances =
        Set.fromList
        . mapMaybe
              (\(sigName, fieldName) ->
                  case
                          parseSigLine
                              (fullSigName (sigName, fieldName))
                              (  Text.unlines
                              $  (maybe [] (\(Expr c) -> [c]) (codeReturn code)
                                 )
                              ++ codeText code
                              )
                      of
                          Nothing        -> Nothing
                          Just (args, _) -> Just (sigName, fieldName, args)
              )
        . Set.toList
        $ signatures
    }

fullSigName :: (SigName, Maybe FieldName) -> FullSigName
fullSigName (SigName sigName, Nothing) = FullSigName sigName
fullSigName (SigName sigName, Just (FieldName fieldName)) = FullSigName $ sigName <> "." <> fieldName

ignore :: Parser ()
ignore = do
    skipSpace
    many' $ do
        choice
            [ (do
                  string "//"
                  takeTill isEndOfLine
                  endOfLine
              )
            , (do
                  string "/*"
                  manyTill anyChar $ string "*/"
                  return ()
              )
            ]
        skipSpace
    return ()

parserTop :: Parser (Maybe Code, [Text], Set Param, Maybe Code, Code, Maybe Code)
parserTop = do
    ignore
    (_, functionsCode) <- option ("", Nothing) $ try $ parserBlock
        "functions"
        (Just <$> parserCode 1)
    ignore
    (_, dataCode) <- option ("", Code [] Nothing)
        $ parserBlock "data" (parserCode 1)
    ignore
    (_, tdCode) <- option ("", Nothing) $ try $ parserBlock
        "transformed data"
        (Just <$> parserCode 1)
    ignore
    topParams <- option Set.empty parserParams
    ignore
    (_, modelCode) <- option ("", Code [] Nothing) $ try $ parserBlock
        "model"
        (parserCode 1)
    ignore
    (_, gqCode) <- option ("", Nothing) $ try $ parserBlock
        "generated quantities"
        (Just <$> parserCode 1)
    ignore
    return
        (functionsCode, codeText dataCode, topParams, tdCode, modelCode, gqCode)

fieldSignatures :: ModuleImplementation code -> Set (SigName, Maybe FieldName)
fieldSignatures moduleImpl =
  Set.fromList [ (implSignature moduleImpl, fieldSignature field) | field <- implFields moduleImpl ]

parserModularProgram :: Parser ModularProgram
parserModularProgram = do
    ignore
    (topFunctions, dataVars, topParams, tdCode, modelCode, gqCode) <- parserTop
    ignore
    implementations <- many' (parserModule <* ignore)
    let signatures = Set.unions $ map fieldSignatures implementations
    return $ ModularProgram
        { signatures      = Set.map ((Type "",) . fst) signatures
        , implementations = Set.map (fmap (findModules signatures))
                                    (Set.fromList implementations)
        , topBody         = findModules signatures modelCode
        , topFunctions    = findModules signatures <$> topFunctions
        , topTD           = findModules signatures <$> tdCode
        , topGQ           = findModules signatures <$> gqCode
        , topData         = dataVars
        , topParams       = topParams
        }

readModularProgram :: MStanFile -> IO ModularProgram
readModularProgram = (parseModularProgram <$>) . Text.readFile . unMStanFile

parseModularProgram :: Text -> ModularProgram
parseModularProgram t = case parseOnly parserModularProgram t of
    Left  remainder -> error $ "\"" ++ remainder ++ "\""
    Right prog      -> prog

parseModularProgram' :: Text -> IResult Text ModularProgram
parseModularProgram' t = parseOnly' parserModularProgram t

parseModularFile :: FilePath -> IO (IResult Text ModularProgram)
parseModularFile = (parseModularProgram' <$>) . Text.readFile

fullTestIRT :: IO (IResult Text ModularProgram)
fullTestIRT = parseModularFile "irt.modular.stan"

fullTestGolf :: IO (IResult Text ModularProgram)
fullTestGolf = parseModularFile "golf.m.stan"

paramTest =
    parseOnly' parserParams $ Text.unlines ["parameters {", "parameters {", "}"]

-- moduleTest = parseOnly parserModule $ Text.unlines [
--     "module \"without-guesses\" IRT_Prob(int i, int j) {"
--   , "  return inv_logit(IRT_Score(i, j));"
--   , "}"
--   ]

parseOnly' parser t = case parse parser t of
    Partial i -> i ""
    x         -> x

codeTest = parseOnly' (parserCode 1) $ Text.unlines ["  testa;", "  testb;"]

codeTest2 = parseOnly' (parserCode 1) $ Text.empty

moduleTest = parseOnly'
    parserModule
    "module \"without-guesses\" IRT_Prob(int i, int j) {}"

moduleTest5 =
    parseOnly' parserModule "module \"Angular\" P(x) {\n  PAngle(x);\n    }"

headTest2 = parseOnly' (moduleHead moduleArgs) "module \"Angular\" P(x)"

-- codeTest5 = parseOnly' parserModule "module \"Angular\" P(x) {\n  PAngle(x);\n    }"

moduleTest4 = parseOnly' parserModule $ Text.unlines
    [ "module \"without-guesses\" IRT_Prob(int i, int j) {"
    , "  testa;"
    , "  testb;"
    , "}"
    ]

moduleTest2 = parseOnly' parserModule $ Text.unlines
    [ "module \"without-guesses\" IRT_Prob(int i, int j) {"
    , "  parameters {"
    , "    real x;"
    , "  }"
    , "  testa;"
    , "  testb;"
    , "}"
    ]

moduleTest6 = parseOnly' parserModule $ Text.unlines
    [ "module \"without-guesses\" IRT_Prob(int i, int j) {"
    , "  parameters {"
    , "    real x;"
    , "  }"
    , "  testa;"
    , "  for (i in 1:I) {"
    , "    testb;"
    , "    testc;"
    , "  }"
    , "  testd;"
    , "}"
    ]

moduleTest3 = parseOnly' parserModule $ Text.unlines
    [ "module \"without-guesses\" IRT_Prob(int i, int j) {"
    , "  parameters {"
    , "    real x;"
    , "  }"
    , "}"
    ]

paramsTest = parseOnly' parserParams
    $ Text.unlines ["  parameters {", "    real x;", "  }"]

bodyTest2 = parseOnly' (moduleBody (parserCode 1))
    $ Text.unlines ["  parameters {", "    real x;", "  }"]

bodyTest4 = parseOnly' (moduleBody (parserCode 1)) $ Text.empty

bodyTest3 =
    parseOnly' (moduleBody (parserCode 1)) $ Text.unlines ["  testa;", "  testb;"]

bodyTest = parseOnly' (moduleBody (parserCode 1)) $ Text.unlines
    ["  parameters {", "    real x;", "  }", "  testa;", "  testb;"]

blockTest = parseOnly'
    (do
        -- parserBlock (string "head") moduleBody
        -- moduleBody
        (parserCode 1)
    )
    "}"

moduleTest' =
    parseOnly'
            (do
                parserBlock (moduleHead moduleArgs) $ do
                    implParams <- option Set.empty parserParams
                    -- ignore
                    implBody   <- option (Code [] Nothing) (parserCode 1)
                    -- ignore
                    return (implParams, implBody)
            )
        $ Text.unlines
              [ "module \"without-guesses\" IRT_Prob(int i, int j) {"
              , "  parameters {"
              , "    real x;"
              , "  }"
              , "  testa;"
        -- , "  testb;"
        -- , "}"
              ]

moduleTest'' =
    parseOnly'
            (do
                parserBlock (moduleHead moduleArgs) $ do
                    implParams <- option Set.empty parserParams
                    ignore
                    implBody <- (parserCode 1)
                    return (implParams, implBody)
            )
        $ Text.unlines
              [ "module \"without-guesses\" IRT_Prob(int i, int j) {"
              , "  parameters {"
              , "    real x;"
              , "  }"
              , "  testa;"
              , "  testb;"
              , "}"
              ]

headTest = parseOnly' (moduleHead moduleArgs)
                      "module \"without-guesses\" IRT_Prob(int i, int j)"

-- this should pass
ignoreTest2 = parseOnly' (ignore >> "hi") $ "    hi"

ignoreTest3 = parseOnly' (skipSpace >> skipSpace >> "hi") $ "    hi"

ignoreTest = parseOnly' ignore $ Text.unlines
    ["   //  data {", "   ", "   // real x;", "", "", "  //}", ""]

ignoreTest4 = parseOnly' ignore $ Text.unlines
    ["   /*  data {", "   ", "   // real x; */ ", "", "", "  //}", ""]

dataTest = parseOnly' (parserBlock "data" (takeTill (== '}')))
    $ Text.unlines ["data {", "  real x;", "}"]

codeTest3 =
    parseOnly' (parserCode 1) $ Text.unlines ["  testa;", "  testb;", "  return 5;"]

codeTest4 = parseOnly' (parserCode 1) $ Text.unlines
    ["  testa; // comment", "", "  testb;", "  return 5; // comment"]

topTest = parseOnly' parserTop $ Text.unlines ["data {", "  real x;", "}"]

topTest2 = parseOnly' parserTop
    $ Text.unlines ["data {", "  real x;", "}", "model {", "  real y;", "}"]

partTest = parseOnly' parserTop $ Text.unlines
    [ "// Version two: heavily abstracted IRT family of models"
    , ""
    , "data {"
    , "  int I; // students"
    , "  int J; // questions"
    , "  array[I, J] int<lower=0, upper=1> y; // 1 if student i answered question j correctly"
    , "}"
    , ""
    , "model {"
    , "  y[i, j] ~ bernoulli(IRT_Prob(i, j));"
    , "}"
    ]

partTest2 = parseOnly' parserTop $ Text.unlines
    [ "// Version two: heavily abstracted IRT family of models"
    , ""
    , "data {"
    , "  int I; // students"
    , "  int J; // questions"
    , "  array[I, J] int<lower=0, upper=1> y; // 1 if student i answered question j correctly"
    , "}"
    , ""
    , "/* asdfasdf "
    , "*/"
    , ""
    , "model {"
    , "  y[i, j] ~ bernoulli(IRT_Prob(i, j));"
    , "}"
    ]

codeBraced = parseOnly' parserBraced $ Text.unlines
      -- codeTest5 = parseOnly' (parserTilClosed' 1) $ Text.unlines
    [ "    peep;"
    , "for (i in 1:I) {"
    , "  for (j in 1:J) {"
    , "    poop;"
    , "  }"
    , "}"
    , ""
    , "}"
    ]

codeBraced2 =
    parseOnly' (choice [string "", parserBraced]) $ Text.unlines
      -- codeTest5 = parseOnly' (parserTilClosed' 1) $ Text.unlines
                                                                 [""]

findTest = findModules (Set.fromList [(SigName "IRT_LogOdds", Nothing)])
                       (Code ["inv_logit(IRT_LogOdds(i, j))"] Nothing)
