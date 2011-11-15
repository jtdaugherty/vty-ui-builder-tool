module Graphics.Vty.Widgets.Builder.Tool
    ( mkBuilderToolMain
    , defaultMain
    )
where

import System
import System.IO
import Control.Monad

import System.Exit
import System.Console.GetOpt
import Text.PrettyPrint.HughesPJ

import Graphics.Vty.Widgets.Builder
import Graphics.Vty.Widgets.Builder.Config
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.DTDGenerator
import Graphics.Vty.Widgets.Builder.Handlers

-- For nicer-looking error message formatting
import Text.Trans.Tokenize (tokenize, wrapStream, serialize)

data BuilderOpt = Help
                | ModuleName String
                | GeneratePreamble Bool
                | GenerateMain Bool
                | GenerateImports Bool
                | OutputFilename String
                | ValidateOnly
                | GenerateInterfaceType Bool
                | GenerateInterfaceBuilder Bool
                | ListSupportedElements
                  deriving (Show, Eq)

options :: [OptDescr BuilderOpt]
options = [ Option "h" ["help"] (NoArg Help) "This help output"

          , Option "n" ["module-name"] (ReqArg ModuleName "NAME")
                       ("The name of the generated module (default: "
                        ++ (show $ moduleName defaultConfig) ++ ")")

          , Option "d" ["no-module-decl"] (NoArg (GeneratePreamble False))
                       ("Do not generate a module declaration or imports (only\n"
                        ++ "generate the module body)")

          , Option "t" ["no-type"] (NoArg (GenerateInterfaceType False))
                       ("Do not generate the interface type used to return\n"
                        ++ "interface elements")

          , Option "f" ["no-function"] (NoArg (GenerateInterfaceBuilder False))
                       "Do not generate the function which builds the interface"

          , Option "m" ["main"] (NoArg (GenerateMain True))
                       ("Generate a \"main\" function for testing (implies\n"
                        ++ "-n \"Main\" and implicit exports)")

          , Option "o" ["output"] (ReqArg OutputFilename "FILENAME")
                       "The output filename (default: standard output)"

          , Option "v" ["validate-only"] (NoArg ValidateOnly)
                       ("Validate the input XML but do not generate any source\n"
                        ++ "code")

          , Option "l" ["list"] (NoArg ListSupportedElements)
                       "List the DTDs of the element types handled by this tool"
          ]

usage :: [String] -> IO ()
usage errs = do
  uh <- usageHeader
  putStrLn $ usageInfo uh options
  mapM_ (putStrLn . ("Error: " ++)) errs

usageHeader :: IO String
usageHeader = do
  progName <- getProgName
  return $ concat [ "Usage: "
                  , progName
                  , " [options] <XML filename>\n"
                  ]

configFromOptions :: [BuilderOpt] -> BuilderConfig
configFromOptions [] = defaultConfig
configFromOptions (o:os) =
    let config = configFromOptions os
    in case o of
         ModuleName s -> config { moduleName = s }
         GeneratePreamble val -> config { generateModulePreamble = val }
         GenerateImports val -> config { generateImports = val }
         GenerateMain val -> config { generateMain = val }
         GenerateInterfaceType val -> config { generateInterfaceType = val }
         GenerateInterfaceBuilder val -> config { generateInterfaceBuilder = val }
         _ -> config

getOutputFilename :: [BuilderOpt] -> Maybe String
getOutputFilename [] = Nothing
getOutputFilename ((OutputFilename s):_) = Just s
getOutputFilename (_:os) = getOutputFilename os

saveOutput :: [BuilderOpt] -> String -> IO ()
saveOutput opts output = do
  case getOutputFilename opts of
    Nothing -> putStrLn output
    Just path -> do
      h <- openFile path WriteMode `catch`
           \e -> do
             putStrLn $ "Error writing output to " ++ show path ++ ":"
             print e
             exitFailure
      hPutStrLn h output
      hClose h
      putStrLn $ "Output written to " ++ show path

listSupportedElements :: [(String, [ElementHandler])] -> IO ()
listSupportedElements theHandlers = do
  let docs = (flip map) theHandlers $ \(dtdPath, hs) ->
             vcat [ text dtdPath <> text ":"
                  , nest 2 $ vcat $
                         map (text . elementName) hs
                  ]
  putStrLn $ render $ vcat docs

mkBuilderToolMain :: [(String, [ElementHandler])] -> IO ()
mkBuilderToolMain theHandlers = do
  args <- getArgs
  let (opts, rest, errs) = getOpt Permute options args

  when (not $ null errs) $ do
         usage errs
         exitFailure

  when (Help `elem` opts) $ do
         usage []
         exitSuccess

  when (ListSupportedElements `elem` opts) $ do
         listSupportedElements theHandlers
         exitSuccess

  when (length rest /= 1) $ usage [] >> exitFailure
  let [xmlFilename] = rest
      config = configFromOptions opts

  inputHandle <- openFile xmlFilename ReadMode `catch`
                 \e -> do
                   putStrLn $ "Error opening " ++ xmlFilename ++ ":"
                   print e
                   exitFailure

  validationResult <- validateAgainstDTD inputHandle xmlFilename theHandlers

  let allElementHandlers = concat $ map snd theHandlers

  case validationResult of
    Left es -> do
         putStrLn $ "Error validating " ++ (show xmlFilename) ++ ":"
         mapM_ putStrLn es
         exitFailure
    Right e -> do
         when (not (ValidateOnly `elem` opts)) $
              do
                result <- generateSourceForDocument config e allElementHandlers
                case result of
                  Left err -> do
                              let errMsg = serialize $ wrapStream 72 $ tokenize err ()
                              putStrLn $ "Error: " ++ errMsg
                              exitFailure
                  Right output -> saveOutput opts output

defaultMain :: IO ()
defaultMain = do
  -- Use the DTD path as dictated by the installation location of this
  -- package.
  dtdPath <- getDTDDir
  mkBuilderToolMain [(dtdPath, elementHandlers)]