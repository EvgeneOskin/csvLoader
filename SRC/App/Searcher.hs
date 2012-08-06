module SRC.App.Searcher ( changeDateFormat
                        , fileValidator
                        , sourceSwitcher
                        , nameToCSV
                        , getNameAndDir
                        , shortSource
) where

import Network.FTP.Client
import Data.Time
import Data.Time.Format
import System.Locale
import Text.Regex.Posix
import Data.Char
import System.FilePath ((</>))
import Data.List.Split

changeDateFormat :: String -> String -> String -> String
changeDateFormat x haveFx needFx = Data.Time.Format.formatTime System.Locale.defaultTimeLocale needFx t
    where t = Data.Time.Format.readTime System.Locale.defaultTimeLocale haveFx x :: Data.Time.UTCTime
              
fileValidator [] _ = []
fileValidator (x:xs) regExpFileName =
    if x =~ regExpFileName :: Bool
      then [x] ++ fileValidator xs regExpFileName
      else fileValidator xs regExpFileName

nameToCSV :: String -> String           
nameToCSV x = (map Data.Char.toLower clearName)  ++ ".csv"
    where clearName = head $ splitOn "." x

getNameAndDir :: String -> String -> String -> String -> (String, String)
getNameAndDir fileName out prefixFileName suffixFileName = (csvFileName, localDir)
    where
      marketDir = map Data.Char.toLower prefixFileName
      dirName = changeDateFormat fileName (prefixFileName ++ suffixFileName) (marketDir ++ "_%Y")
      localDir = (out </>  marketDir) </> dirName
      csvFileName = localDir </> (SRC.App.Searcher.nameToCSV fileName)

sourceSwitcher :: String -> ((String, (String, String)), [String])
sourceSwitcher source =
    case source of
      "eoddata" -> (("ftp.eoddata.com", ("_[0-9]{8}.txt", "_%Y%m%d.txt")), ["/History","/"])
      _ -> (("",("","")), [""])
      -- (("ftp.server.name", ("csv file suffix for regexp", "csv file suffix for readTime", ), ["dirs/where/it/can/find/csv/file"])

shortSource :: String -> String
shortSource source =
    case source of
      "eoddata" -> "EOD_"
      _ -> ""
