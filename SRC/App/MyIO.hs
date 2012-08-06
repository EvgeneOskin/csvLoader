module SRC.App.MyIO ( processArgs
                    , run
                    , Options(..)
                    , connector
                    , loader
                    , searchCSV
                    , copyCSVSingle
                    , preparation
) where

import System.Environment (getArgs, getEnv)
import Network.FTP.Client
import Text.Regex.Posix
import System.FilePath ((</>))
import System.Directory
import Data.Char
import SRC.App.Searcher
import System.Console.GetOpt
import Control.Monad (forM)
import Data.List.Split

processArgs :: [String] -> IO (Options, [String])
processArgs argv =
   case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
   where header = "Usage: csvLoader [OPTION...]"

data Options = Options
 { optMarket      :: String
 , optOuputDir    :: String
 , optSourceFTP   :: String
 , optUserFTP     :: String
 , optPasswordFTP :: String
 , optAccountFTP  :: String
 , optHelp        :: Bool
 } deriving Show

defaultOptions :: Options
defaultOptions    = Options
 { optMarket      = ""
 , optOuputDir    = ""
 , optSourceFTP   = ""
 , optUserFTP     = ""
 , optPasswordFTP = ""
 , optAccountFTP  = ""
 , optHelp        = False
 }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['M'] ["market"]
              (ReqArg (\ f opts -> opts { optMarket     = f })
                          "STR") "markets name"
 , Option ['O'] ["output_dir"]
              (ReqArg (\ f opts -> opts { optOuputDir   = f })
                         "STR") "output directory"
 , Option ['S'] ["source"]
              (ReqArg (\ f opts -> opts { optSourceFTP  = f })
                         "STR") "ftp server"
 , Option ['U'] ["user"]
              (ReqArg (\ f opts -> opts { optUserFTP    = f })
                         "STR") "user\'s name"
 , Option ['P'] ["password"]
              (ReqArg (\ f opts -> opts { optPasswordFTP = f })
                         "STR") "user\'s password"
 , Option ['A'] ["account"]
              (ReqArg (\ f opts -> opts { optAccountFTP  = f })
                         "STR") "user\'s account type"
 , Option ['H'] ["help"]
              (NoArg (\   opts -> opts { optHelp        = True })
              ) "help"
 ]

run (Options _ _ _ _ _ _ True, _)  = do
  putStrLn $ usageInfo "" options

run (Options markets out sourceFtp "" "" "" _, _)  = do
  let shrtSrc = shortSource sourceFtp
  user <- getEnv $ shrtSrc ++ "USER"
  pass <- getEnv $ shrtSrc ++ "PASSWD"
  loader (endBy ","  markets) out sourceFtp user (Just pass) Nothing

run (Options markets out sourceFtp user pass "" _, _)  =
    loader (endBy "," markets) out sourceFtp user (Just pass) Nothing
           
run (Options markets out sourceFtp user pass account _, _) =
    loader (endBy "," markets) out sourceFtp user (Just pass) (Just account)
    
loader :: [String] -> String -> String -> String -> Maybe String -> Maybe String -> IO ()
loader markets out sourceFtp userName password account = do
  curDir <- getCurrentDirectory
  putStrLn curDir
  tempDir <- preparation     
  setCurrentDirectory tempDir
  putStrLn tempDir
  let ftpData = sourceSwitcher sourceFtp
  forM markets $ \market -> do
    hftp <- connector (fst $ fst ftpData) userName password account
    searchCSV hftp (snd ftpData) market (snd $ fst ftpData) (out </> sourceFtp) tempDir
    quit hftp
  removeDirectoryRecursive tempDir
  setCurrentDirectory curDir
  return ()

searchCSV :: FTPConnection -> [String] -> String -> (String, String) -> String -> String -> IO FTPConnection
searchCSV hftp []     _              _                            _   _       = return hftp
searchCSV hftp (x:xs) prefixFileName (fileNameRegExp, dateRegExp) out tempDir = do
  Network.FTP.Client.cwd hftp x
  dirContent <- nlst hftp Nothing
  let csvFiles = fileValidator dirContent (prefixFileName ++ fileNameRegExp)
  putStrLn $ unlines csvFiles
  forM csvFiles $ \name -> do
    let nameAndDir = getNameAndDir name out prefixFileName dateRegExp
    isExist <- System.Directory.doesFileExist $ fst nameAndDir
    if isExist
      then
          return ()
      else do
        print name
        Network.FTP.Client.downloadbinary hftp name
        copyCSVSingle (tempDir </> name) nameAndDir
        return ()
  searchCSV hftp xs prefixFileName (fileNameRegExp, dateRegExp) out tempDir

copyCSVSingle :: String -> (String, String) -> IO ()
copyCSVSingle filePath (csvFileName, localDir)= do
  putStrLn csvFileName
  System.Directory.createDirectoryIfMissing True localDir
  System.Directory.copyFile filePath csvFileName

preparation :: IO String
preparation = do
  sysTemp <- getTemporaryDirectory
  let tempDir = sysTemp </> "CSVLoader"
  doesExist <- doesDirectoryExist tempDir
  if doesExist
    then do
      System.Directory.removeDirectoryRecursive tempDir
      System.Directory.createDirectoryIfMissing True tempDir
      return tempDir
    else do
      System.Directory.createDirectoryIfMissing True tempDir
      return tempDir

connector :: String -> String -> Maybe String -> Maybe String -> IO FTPConnection
connector ftpServer userName password account = do
  hftp <- Network.FTP.Client.easyConnectFTP ftpServer
  Network.FTP.Client.login hftp userName password account
  return hftp

