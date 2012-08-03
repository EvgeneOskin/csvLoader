module SRC.App.MyIO
    (
     inputGreeting,
     chooser,
     Flag(..),
     connector,
     loader,
     searchCSV,
     copyCSVSingle,
     preparation
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

    inputGreeting = do
      args <- getArgs
      case getOpt RequireOrder options args of
        (flags, nonOpts, msgs) -> do
                      print flags
                      chooser flags

    data Flag = Market String
              | OuputDir String
              | SourceFTP String
              | UserFTP String
              | PasswordFTP String
              | AccountFTP String
              | Help
              deriving Show
                
    options = [ Option ['M'] ["market"] (ReqArg Market "NAME") "martek name",
                Option ['O'] ["output_dir"] (ReqArg OuputDir "DIR") "output directory",
                Option ['S'] ["source"] (ReqArg SourceFTP "FTP") "ftp server",
                Option ['U'] ["user"] (ReqArg UserFTP "NAME") "user\'s name",
                Option ['P'] ["password"] (ReqArg PasswordFTP "WORD") "user\'s password",
                Option ['A'] ["account"] (ReqArg AccountFTP "TYPE") "user\'s account type",
                Option ['H'] ["help"] (NoArg Help) "help"
              ]

    chooser [Market markets, OuputDir out, SourceFTP sourceFtp, UserFTP user, PasswordFTP pass, AccountFTP account] =
        loader (makeMarkets markets) out sourceFtp user (Just pass) (Just account)

    chooser [Market markets, OuputDir out, SourceFTP sourceFtp, UserFTP user, PasswordFTP pass] =
        loader (makeMarkets markets) out sourceFtp user (Just pass) Nothing

    chooser [Market markets, OuputDir out, SourceFTP sourceFtp] = do
      let shrtSrc = shortSource sourceFtp
      user <- getEnv $ shrtSrc ++ "USER"
      pass <- getEnv $ shrtSrc ++ "PASSWD"
      loader (makeMarkets markets) out sourceFtp user (Just pass) Nothing

    chooser [Help] = do
      putStrLn $ "Version 1.0\nFlags:\n" ++ marketFlag ++ outdirFlag
                   ++ sourceFlag ++ userFlag ++ passWFlag ++ accntFlag
                   ++ helpFlag
          where marketFlag = "   -M [--market] markets_names\n"
                outdirFlag = "   -O [--output_dir] path/to/output/directory\n"
                sourceFlag = "   -S [--source] ftp_server_name (supported:\       \'eodata\')\n"
                userFlag   = "   -U [--user] user_name\n"
                passWFlag  = "   -P [--password] user_password\n"
                accntFlag  = "   -A [--account] user_account-type\n"
                helpFlag   = "   -H [--help] (you\'ll see this message)"
    chooser _ = chooser [Help]
                             
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

    searchCSV hftp    []     _              _             _   _       = return hftp
    searchCSV hftp (x:xs) prefixFileName suffisRegExps out tempDir = do
      Network.FTP.Client.cwd hftp x
      dirContent <- nlst hftp Nothing
      let csvFiles = fileValidator dirContent (prefixFileName ++ (fst suffisRegExps))
      putStrLn $ unlines csvFiles
      forM csvFiles $ \name -> do
        let nameAndDir = getNameAndDir name out prefixFileName (snd suffisRegExps)
        isExist <- System.Directory.doesFileExist $ fst nameAndDir
        if isExist
          then
            return ()
          else do
            print name
            Network.FTP.Client.downloadbinary hftp name
            copyCSVSingle (tempDir </> name) (fst nameAndDir) (snd nameAndDir)
            return ()
      searchCSV hftp xs prefixFileName suffisRegExps out tempDir

    copyCSVSingle filePath csvFileName localDir= do
      putStrLn csvFileName
      System.Directory.createDirectoryIfMissing True localDir
      System.Directory.copyFile filePath csvFileName

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

    connector ftpServer userName password account = do
      hftp <- Network.FTP.Client.easyConnectFTP ftpServer
      Network.FTP.Client.login hftp userName password account
      return hftp

