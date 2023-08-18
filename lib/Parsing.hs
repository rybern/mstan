{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsing where

import           Data.Attoparsec.Text
import           Data.List                      ( nub )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Function
import           Data.Char


import           SemanticChecking
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

parseSigLine :: FullSigName -> Text -> Maybe ([Expr], Maybe (Expr -> Text))
parseSigLine sigName line = case parseOnly (parserSigLine sigName) line of
    Left  _   -> Nothing
    Right res -> Just res

parserSigLine :: FullSigName -> Parser ([Expr], Maybe (Expr -> Text))
parserSigLine (FullSigName sigName) = choice
  [ do
      left <- manyTill anyChar $ char '~'
      let samplee = Expr . Text.strip . Text.pack $ left
      skipSpace
      _ <- string sigName
      skipSpace
      args  <- parserArgs
      skipSpace
      _ <- char ';'
      return (samplee : args, Nothing)
  , do
      skipSpace
      _ <- string sigName
      skipSpace
      args  <- parserArgs
      skipSpace
      _ <- char ';'
      return (args, Nothing)
  , do
      left  <- manyTill anyChar $ string sigName
      skipSpace
      args  <- parserArgs
      right <- many' anyChar
      return (args, Just $ \(Expr x) -> Text.pack left <> x <> Text.pack right)
  ]

parseVar :: Parser Text
parseVar = do
  skipSpace
  content <- takeWhile isVariableChar
  skipSpace
  return content

parseParenthetical :: Parser [Expr]
parseParenthetical = do
  skipSpace
  content <- parserArgs
  skipSpace
  return content

maybeParse :: Parser a -> Parser ()
maybeParse p = choice [p *> return (), return ()]

needsParentheses :: Text -> Bool
needsParentheses line = case parseOnly boundParser line of
    Left  _ -> True
    Right _ -> False
  where boundParser = do
          maybeParse parseVar
          maybeParse parseParenthetical
          endOfInput

maybeAddParens :: Expr -> Expr
maybeAddParens (Expr e) = Expr $ if needsParentheses e then "(" <> e <> ")" else e

isVariableChar :: Char -> Bool
isVariableChar c = isAlphaNum c || c == '_'

replaceCountVar :: Text -> Text -> Text -> (Int, Text)
replaceCountVar var var' line = (n, line')
  where tokens = Text.groupBy ((==) `on` isVariableChar) line
        n = length . filter (== var) $ tokens
        line' = Text.concat . map (\word -> if word == var then var' else word) $ tokens

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
            _ <- char ';'
            ignore
            (lines', ret) <- matchLines
            return $ (line <> ";" : lines', ret)
        matchReturn = do
            ignore
            _ <- string "return "
            expr <- takeTill (inClass "{};")
            _ <- char ';'
            ignore
            return ([], Just (Expr expr))
        matchBraced = do
            ignore
            braced <- parserBraced
            ignore
            (lines', ret) <- matchLines
            return (braced : lines', ret)
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
    (lines', codeReturn) <- matchLines
    return $ Code { codeText = unindentNestedLines expectedIndent lines', codeReturn = codeReturn }

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

parserModel :: Parser Code
parserModel = do
    ignore
    (_, tp) <- parserBlock (string "model") (parserCode 2)
    return tp

parserTP :: Parser Code
parserTP = do
    ignore
    (_, tp) <- parserBlock (string "transformed parameters") (parserCode 2)
    return tp

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
    ((implName, implSignature, ()), (functions, params, td, tp, model, gq, implFields)) <-
        parserBlock (moduleHead (return ())) (moduleBody parserFields)
    let implBlocks = Blocks { ..}
    return $ ModuleImplementation { .. }

parserSingletonModule :: Parser (ModuleImplementation Code)
parserSingletonModule = do
    ((implName, implSignature, implArgs), (functions, params, td, tp, model, gq, implBody)) <-
        parserBlock (moduleHead moduleArgs) (moduleBody (parserCode 1))
    let implFields =
            [ ModuleField { fieldBody      = implBody
                          , fieldSignature = Nothing
                          , fieldArgs      = implArgs
                          }
            ]
    let implBlocks = Blocks { ..}
    return $ ModuleImplementation { .. }

moduleArgs :: Parser [Text]
moduleArgs = do
    skipSpace
    _ <- char '('
    implArgs <- sepBy (takeTill (inClass "|),")) (choice [",", "|"])
    _ <- char ')'
    return $ map Text.strip implArgs

moduleHead :: Parser args -> Parser (ImplName, SigName, args)
moduleHead parseArgs = do
    _ <- string "module"
    skipSpace
    _ <- char '"'
    implName <- takeTill (== '"')
    _ <- char '"'
    skipSpace
    sigName  <- takeTill (inClass "( {")
    implArgs <- parseArgs
    return (ImplName implName, SigName $ Text.strip sigName, implArgs)

moduleBody
    :: Parser body
    -> Parser (Maybe Code, Set Param, Maybe Code, Maybe Code, Maybe Code, Maybe Code, body)
moduleBody parseBody = do
    implFunctions <- option Nothing (Just <$> parserFunctions)
    ignore
    implTD <- option Nothing (Just <$> parserTD)
    ignore
    implParams <- option Set.empty parserParams
    ignore
    implTP <- option Nothing (Just <$> parserTP)
    ignore
    implModel <- option Nothing (Just <$> parserModel)
    ignore
    implGQ <- option Nothing (Just <$> parserGQ)
    ignore
    implBody <- parseBody
    return (implFunctions, implParams, implTD, implTP, implModel, implGQ, implBody)

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
    _ <- many' $ do
        choice
            [ (do
                  _ <- string "//"
                  _ <- takeTill isEndOfLine
                  endOfLine
              )
            , (do
                  _ <- string "/*"
                  _ <- manyTill anyChar $ string "*/"
                  return ()
              )
            ]
        skipSpace
    return ()

parserTop :: Parser (Maybe Code, [Text], Set Param, Maybe Code, Maybe Code, Code, Maybe Code)
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
    (_, tpCode) <- option ("", Nothing) $ try $ parserBlock
        "transformed parameters"
        (Just <$> parserCode 1)
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
        (functionsCode, codeText dataCode, topParams, tdCode, tpCode, modelCode, gqCode)

fieldSignatures :: ModuleImplementation code -> Set (SigName, Maybe FieldName)
fieldSignatures moduleImpl =
  Set.fromList [ (implSignature moduleImpl, fieldSignature field) | field <- implFields moduleImpl ]

parserModularProgram :: Parser ModularProgram
parserModularProgram = do
    ignore
    (topFunctions, dataVars, topParams, tdCode, tpCode, modelCode, gqCode) <- parserTop
    ignore
    implementations <- many' (parserModule <* ignore)
    -- let signatures = map ((Type "",) . implSignature) implementations
    let allFieldSignatures = Set.unions $ map fieldSignatures implementations
        signatures = nub $ map ((Type "",) . implSignature) implementations
    return $ ModularProgram
        { signatures      = signatures
        , implementations = map (fmap (findModules allFieldSignatures)) implementations
        , topProgram      = Program {
              progData         = dataVars
            , progBlocks       = Blocks {
                  functions    = findModules allFieldSignatures <$> topFunctions
                , td           = findModules allFieldSignatures <$> tdCode
                , tp           = findModules allFieldSignatures <$> tpCode
                , model        = Just $ findModules allFieldSignatures modelCode
                , gq           = findModules allFieldSignatures <$> gqCode
                , params       = topParams
                }
            }
        }

readModularProgram :: MStanFile -> IO (Either ProgramError ModularProgram)
readModularProgram = (parseModularProgram <$>) . Text.readFile . unMStanFile

data ProgramError = SyntaxError {
  linesRemaining :: Int
  , contexts :: [String]
  , description :: String
  }
  | ErrorUnfinishedParse
  | SemanticError [SemanticError]
             deriving Show

showProgramError :: ProgramError -> [String]
showProgramError ErrorUnfinishedParse = ["Unexpected extra symbols; expected end of input"]
showProgramError (SyntaxError {..}) = ["Error " ++ show linesRemaining ++ " from end of input:"
                                      , "  " ++ description
                                      , "at points: "] ++ contexts
showProgramError (SemanticError es) = concatMap showSemanticError es

-- parseModularProgram :: Text -> Either ErrorReport ModularProgram
-- parseModularProgram t = case parseOnly parserModularProgram t of
--     Left description ->
--       Left $ SyntaxError 0 [] description
--     Right prog      -> Right prog

parseModularProgram :: Text -> Either ProgramError ModularProgram
parseModularProgram t = case parse parserModularProgram t of
    Fail r c d-> handleFail r c d
    Partial f -> case f mempty of 
      Fail r c d-> handleFail r c d
      Partial _ -> error "Should not find partial twice"
      Done r p      -> handleDone r p
    -- Partial _ -> Left $ ErrorDidNotFinish
    Done r p      -> handleDone r p
  where handleFail remainder contexts description = 
          Left $ SyntaxError (length . Text.lines $ remainder) contexts description
        handleDone "" prog = case (semanticCheck prog) of
          [] -> Right prog
          errs -> Left (SemanticError errs)
        handleDone _ _ = Left ErrorUnfinishedParse

{-
Old parser test code. Should turn into a test suite

parseModularFile :: FilePath -> IO (IResult Text ModularProgram)
parseModularFile = (parseModularProgram' <$>) . Text.readFile

parseModularProgram' :: Text -> IResult Text ModularProgram
parseModularProgram' t = parseOnly' parserModularProgram t

fullTestIRT :: IO (IResult Text ModularProgram)
fullTestIRT = parseModularFile "irt.modular.stan"

fullTestGolf :: IO (IResult Text ModularProgram)
fullTestGolf = parseModularFile "golf.m.stan"

parseOnly' :: Parser a -> Text -> IResult Text a
parseOnly' parser t = case parse parser t of
    Partial i -> i ""
    x         -> x


-- moduleTest = parseOnly parserModule $ Text.unlines [
--     "module \"without-guesses\" IRT_Prob(int i, int j) {"
--   , "  return inv_logit(IRT_Score(i, j));"
--   , "}"
--   ]

paramTest :: IResult Text (Set Param)
paramTest =
    parseOnly' parserParams $ Text.unlines ["parameters {", "parameters {", "}"]

tests :: IO ()
tests = mapM_
    (\(sig, code) -> do
        let Just (args, Just f) = fromJust $ parseSigLine (FullSigName sig) code
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
    [ "    x;"
    , "for (i in 1:I) {"
    , "  for (j in 1:J) {"
    , "    x;"
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

-}
