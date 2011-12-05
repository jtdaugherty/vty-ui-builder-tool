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
import System.FilePath (takeExtension)

import Graphics.Vty.Widgets.Builder
import Graphics.Vty.Widgets.Builder.Config
import Graphics.Vty.Widgets.Builder.Types
import Graphics.Vty.Widgets.Builder.Handlers (coreSpecHandlers)

import Graphics.Vty.Widgets.Builder.Reader
import Graphics.Vty.Widgets.Builder.Reader.XML

data BuilderOpt = Help
                | ModuleName String
                | GenerateMain Bool
                | OutputFilename String
                | ValidateOnly
                | GenerateInterfaceType Bool
                | GenerateInterfaceBuilder Bool
                  deriving (Show, Eq)

options :: [OptDescr BuilderOpt]
options = [ Option "h" ["help"] (NoArg Help) "This help output"

          , Option "n" ["module-name"] (ReqArg ModuleName "NAME")
                       ("The name of the generated module (default: "
                        ++ (show $ moduleName defaultConfig) ++ ")")

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
                       ("Validate the input document but do not generate any source\n"
                        ++ "code")
          ]

usage :: [String] -> IO ()
usage errs = do
  uh <- usageHeader
  putStrLn $ usageInfo uh options
  putStrLn $ usageFooter inputReaders
  mapM_ (putStrLn . ("Error: " ++)) errs

usageFooter :: [(String, (String, DocumentReader))] -> String
usageFooter rs = unlines $ "Supported input document types:"
                 : (map (\(ext, (desc, _)) -> "  " ++ ext ++ " - " ++ desc) rs)

usageHeader :: IO String
usageHeader = do
  progName <- getProgName
  return $ concat [ "Usage: "
                  , progName
                  , " [options] <input document>\n"
                  ]

configFromOptions :: [BuilderOpt] -> BuilderConfig
configFromOptions [] = defaultConfig
configFromOptions (o:os) =
    let config = configFromOptions os
    in case o of
         ModuleName s -> config { moduleName = s }
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

mkBuilderToolMain :: [(String, (String, DocumentReader))]
                  -> [WidgetElementHandler]
                  -> IO ()
mkBuilderToolMain readers specHandlers = do
  args <- getArgs
  let (opts, rest, errs) = getOpt Permute options args

  when (not $ null errs) $ do
         usage errs
         exitFailure

  when (Help `elem` opts) $ do
         usage []
         exitSuccess

  when (length rest /= 1) $ usage [] >> exitFailure
  let [inputFilename] = rest
      config = configFromOptions opts
      ext = takeExtension inputFilename

  reader <- case lookup ext readers of
              Nothing -> do
                putStrLn $ "Error: no input document reader found for file extension " ++ (show ext)
                exitFailure
              Just (_, r) -> return r

  docResult <- readDocument reader inputFilename `catch`
               \e -> do
                 putStrLn $ "Error opening " ++ inputFilename ++ ":"
                 print e
                 exitFailure

  doc <- case docResult of
           Left es -> do
             putStrLn $ "Error processing " ++ (show inputFilename) ++ ":\n"
             forM_ es $ \(msg, pos) ->
                 putStrLn $ show pos ++ ":\n  " ++ msg ++ "\n"
             exitFailure
           Right d -> return d

  result <- generateSourceForDocument config doc specHandlers
  case result of
    Left generationErrors ->
        do
          putStrLn $ "Error in source generation:"
          mapM_ print generationErrors
          exitFailure
    Right output -> if ValidateOnly `elem` opts then
                        putStrLn "Validation succeeded, not writing source" else
                        saveOutput opts (prettyPrintSource output)

-- |Filename extension, description, reader.
inputReaders :: [(String, (String, DocumentReader))]
inputReaders = [ (".xml"
                 , ( "HaXml-based document reader (vty-ui-builder-xml package)"
                   , xmlReader
                   )
                 )
               ]

defaultMain :: IO ()
defaultMain = mkBuilderToolMain inputReaders coreSpecHandlers