{-# LANGUAGE OverloadedStrings #-}
module Parsing where

import           Types
import           Prelude                 hiding ( takeWhile )
import           Data.Attoparsec.Text
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Monoid
import           Data.Maybe
import           Control.Monad
import           Debug.Trace

parseSelections :: Text -> Maybe (Map SigName ImplName)
parseSelections s = case parseOnly parserSelections s of
    Left  remainder -> Nothing
    Right res       -> Just res

parserSelections :: Parser (Map SigName ImplName)
parserSelections = do
    let pair = do
            sig <- takeTill (inClass ",:")
            char ':'
            impl <- takeTill (inClass ",:")
            return (sig, impl)
    pairs <- sepBy pair ","
    return $ Map.fromList pairs


parseSigLine :: SigName -> Text -> Maybe ([Expr], Expr -> Text)
parseSigLine sigName line = case parseOnly (parserSigLine sigName) line of
    Left  remainder -> Nothing
    Right res       -> Just res

parserSigLine :: SigName -> Parser ([Expr], Expr -> Text)
parserSigLine sigName = do
    left  <- manyTill anyChar $ string sigName
    args  <- parserArgs
    right <- many' anyChar
    return (args, \(Expr x) -> Text.pack left <> x <> Text.pack right)

parserArgs :: Parser [Expr]
parserArgs = do
    char '('
    -- args <- sepBy "," (many' (notChar ')'))
    -- args <- (many' (notChar ')'))
    args <- sepBy (parserTilClosed 0) (choice [",", "|"])
    char ')'
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

tests = mapM_
    (\(sig, code) -> do
        let Just (args, f) = parseSigLine sig code
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

parserBlock :: Parser head -> Parser body -> Parser (head, body)
parserBlock parserHead parserBody = do
    head <- parserHead
    skipSpace
    char '{'
    skipSpace
    body <- parserBody
    skipSpace
    char '}'
    skipSpace
    return (head, body)

parserBraced :: Parser Text
parserBraced = do
    line <- takeWhile (notInClass "{}")
    char '{'
    ((line <> "{") <>) <$> parserTilClosed' 1

parserTilClosed' :: Int -> Parser Text
parserTilClosed' 0    = return ""
parserTilClosed' open = do
    block <- takeWhile (notInClass "{}")
    peek  <- peekChar
    return $ Text.pack $ (maybe [] (\p -> [p]) peek)

    let open' = traceShow open . traceShow block . traceShow peek $ open
    choice
        [ char '}' >> (((block <> "}") <>) <$> parserTilClosed' (open - 1))
        , char '{' >> (((block <> "{") <>) <$> parserTilClosed' (open + 1))
        ]

parserCode :: Parser Code
parserCode = do
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
    return $ Code { codeText = lines, codeReturn = codeReturn }

parserGQ :: Parser Code
parserGQ = do
    ignore
    (_, gq) <- parserBlock (string "generated quantities") parserCode
    return gq

parserParams :: Parser (Set Param)
parserParams = do
    ignore
    (_, params) <- parserBlock (string "parameters") $ do
        params <- many' $ takeTill (inClass "};") <* char ';'
        return . Set.fromList . map (Param . Text.strip) $ params
    return params

parserModule :: Parser (ModuleImplementation Code)
parserModule = do
    ((implName, implSignature, implArgs), (implParams, implGQ, implBody)) <- parserBlock
        moduleHead
        moduleBody
    return $ ModuleImplementation { implBody      = implBody
                                  , implArgs      = implArgs
                                  , implSignature = implSignature
                                  , implGQ        = implGQ
                                  , implParams    = implParams
                                  , implName      = implName
                                  }

moduleHead :: Parser (Text, Text, [Text])
moduleHead = do
    string "module"
    skipSpace
    char '"'
    implName <- takeTill (== '"')
    char '"'
    skipSpace
    sigName <- takeTill (== '(')
    char '('
    implArgs <- sepBy (takeTill (inClass "|),")) (choice [",", "|"])
    char ')'
    return (implName, Text.strip sigName, map Text.strip implArgs)

moduleBody :: Parser (Set Param, Maybe Code, Code)
moduleBody = do
    implParams <- option Set.empty parserParams
    ignore
    implGQ <- option Nothing (Just <$> parserGQ)
    ignore
    implBody <- parserCode
    return (implParams, implGQ, implBody)

findModules :: Set (Type, SigName) -> Code -> ModularCode
findModules signatures code = ModularCode
    { modularCode     = code
    , moduleInstances =
        Set.fromList
        . mapMaybe
              (\(_, sigName) ->
                  case
                          parseSigLine
                              sigName
                              (  Text.unlines
                              $  (maybe [] (\(Expr c) -> [c]) (codeReturn code)
                                 )
                              ++ codeText code
                              )
                      of
                          Nothing        -> Nothing
                          Just (args, _) -> Just (sigName, Nothing, args)
              )
        . Set.toList
        $ signatures
    }

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

parserTop :: Parser ([Text], Code, Code)
parserTop = do
    ignore
    (_, dataCode) <- option ("", Code [] Nothing)
        $ parserBlock "data" parserCode
    ignore
    (_, modelCode) <- option ("", Code [] Nothing) $ try $ parserBlock
        "model"
        parserCode
    ignore
    (_, gqCode) <- option ("", Code [] Nothing) $ try $ parserBlock
        "generated quantities"
        parserCode
    ignore
    return (codeText dataCode, modelCode, gqCode)

parserModularProgram :: Parser ModularProgram
parserModularProgram = do
    ignore
    topParams <- option Set.empty parserParams
    ignore
    (dataVars, modelCode, gqCode) <- parserTop
    ignore
    implementations <- Set.fromList <$> many' (parserModule <* ignore)
    let signatures =
            Set.map (\impl -> (Type "", implSignature impl)) implementations
    return $ ModularProgram
        { signatures      = signatures
        , implementations = Set.map (fmap (findModules signatures)) implementations
        , topBody         = findModules signatures modelCode
        , topGQ           = findModules signatures gqCode
        , topData         = dataVars
        , topParams       = topParams
        }

readModularProgram :: FilePath -> IO ModularProgram
readModularProgram = (parseModularProgram <$>) . Text.readFile

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

codeTest = parseOnly' parserCode $ Text.unlines ["  testa;", "  testb;"]
codeTest2 = parseOnly' parserCode $ Text.empty

moduleTest = parseOnly'
    parserModule
    "module \"without-guesses\" IRT_Prob(int i, int j) {}"

moduleTest5 =
    parseOnly' parserModule "module \"Angular\" P(x) {\n  PAngle(x);\n    }"
headTest2 = parseOnly' moduleHead "module \"Angular\" P(x)"
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

bodyTest2 = parseOnly' moduleBody
    $ Text.unlines ["  parameters {", "    real x;", "  }"]

bodyTest4 = parseOnly' moduleBody $ Text.empty

bodyTest3 = parseOnly' moduleBody $ Text.unlines ["  testa;", "  testb;"]

bodyTest = parseOnly' moduleBody $ Text.unlines
    ["  parameters {", "    real x;", "  }", "  testa;", "  testb;"]

blockTest = parseOnly'
    (do
        -- parserBlock (string "head") moduleBody
        -- moduleBody
        parserCode
    )
    "}"

moduleTest' =
    parseOnly'
            (do
                parserBlock moduleHead $ do
                    implParams <- option Set.empty parserParams
                    -- ignore
                    implBody   <- option (Code [] Nothing) parserCode
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
                parserBlock moduleHead $ do
                    implParams <- option Set.empty parserParams
                    ignore
                    implBody <- parserCode
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

headTest =
    parseOnly' moduleHead "module \"without-guesses\" IRT_Prob(int i, int j)"

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
    parseOnly' parserCode $ Text.unlines ["  testa;", "  testb;", "  return 5;"]

codeTest4 = parseOnly' parserCode $ Text.unlines
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

findTest = findModules (Set.fromList [(Type "", "IRT_LogOdds")])
                       (Code ["inv_logit(IRT_LogOdds(i, j))"] Nothing)
