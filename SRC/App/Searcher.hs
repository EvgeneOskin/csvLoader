module SRC.App.Searcher
    (
     makeGoodDate,
     fileValidator,
     sourceSwitcher,
     nameToCSV,
     getNameAndDir,
     makeMarkets,
     shortSource
    ) where

    import Network.FTP.Client
    import Data.Time
    import Data.Time.Format
    import System.Locale
    import Text.Regex.Posix
    import Data.Char
    import System.FilePath ((</>))
        
    makeGoodDate x haveFx needFx = Data.Time.Format.formatTime System.Locale.defaultTimeLocale needFx t
        where t = Data.Time.Format.readTime System.Locale.defaultTimeLocale haveFx x :: Data.Time.UTCTime

    fileDateCompare fileName1 fileName2 marketName sourceDateType = date2 > date1
        where date2 = Data.Time.Format.readTime System.Locale.defaultTimeLocale timeType fileName2 :: UTCTime
              date1 = Data.Time.Format.readTime System.Locale.defaultTimeLocale timeType fileName1 :: UTCTime
              timeType = marketName ++ "_" ++ sourceDateType

    separateStringFirst _ [] = []                                            
    separateStringFirst f (x:xs)
        | f x = []
        | otherwise = x:separateStringFirst f xs

    separateStringMulti _ [] = []                         
    separateStringMulti f xs = lx : separateStringMulti f lxs
        where
          lx = separateStringFirst f xs
          lxs = drop (length lx + 1) xs
                      
    fileValidator [] _ = []
    fileValidator (x:xs) regExpFileName =
        if x =~ regExpFileName :: Bool
          then [x] ++ fileValidator xs regExpFileName
          else fileValidator xs regExpFileName

    nameToCSV x = (map Data.Char.toLower clearName)  ++ ".csv"
        where clearName = separateStringFirst (=='.') x

    getNameAndDir fileName out prefixFileName suffixFileName = (csvFileName, localDir)
        where
          marketDir = map Data.Char.toLower prefixFileName
          dirName = makeGoodDate fileName (prefixFileName ++ suffixFileName) (marketDir ++ "_%Y")
          localDir = (out </>  marketDir) </> dirName
          csvFileName = localDir </> (SRC.App.Searcher.nameToCSV fileName)
                        
    sourceSwitcher source =
        case source of
          "eoddata" -> (("ftp.eoddata.com", ("_[0-9]{8}.txt", "_%Y%m%d.txt")), ["/History","/"])
          _ -> (("",("","")), [""])
          -- (("ftp.server.name", ("csv file suffix for regexp", "csv file suffix for readTime", ), ["dirs/where/it/can/find/csv/file"])

    shortSource source =
        case source of
          "eoddata" -> "EOD_"
          _ -> ""               
    makeMarkets x = separateStringMulti (==',') x