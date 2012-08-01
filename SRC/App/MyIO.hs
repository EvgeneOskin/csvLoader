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

    import System.Environment (getArgs)
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

    chooser [Market market, OuputDir out, SourceFTP sourceFtp, UserFTP user, PasswordFTP pass, AccountFTP account] =
        loader market out sourceFtp user pass account

    chooser [Help] = do
      putStrLn $ "Version 0.0\nFlags:\n" ++ marketFlag ++ outdirFlag
                   ++ sourceFlag ++ userFlag ++ passWFlag ++ accntFlag
                   ++ helpFlag
          where marketFlag = "   -M [--market] market_name\n"
                outdirFlag = "   -O [--output_dir] path/to/output/directory\n"
                sourceFlag = "   -S [--source] ftp_server_name (supported:\       \'eodata\')\n"
                userFlag   = "   -U [--user] user_name\n"
                passWFlag  = "   -P [--password] user_password\n"
                accntFlag  = "   -A [--account] user_account-type\n"
                helpFlag   = "   -H [--help] (you\'ll see this message)"
    chooser _ = chooser [Help]
                             
    loader market out sourceFtp userName password account = do
      curDir <- getCurrentDirectory
      tempDir <- preparation     
      setCurrentDirectory tempDir
      let ftpData = sourceSwitcher sourceFtp
      hftp <- connector (fst $ fst ftpData) userName password account
      searchCSV hftp (snd ftpData) market (snd $ fst ftpData) out tempDir
      quit hftp
      removeDirectoryRecursive tempDir
      setCurrentDirectory curDir

    searchCSV _    []     _              _             _   _       = return ()
    searchCSV hftp (x:xs) prefixFileName suffisRegExps out tempDir = do
      Network.FTP.Client.cwd hftp x
      dirContent <- nlst hftp Nothing
      let csvFiles = fileValidator dirContent (prefixFileName ++ (fst suffisRegExps))
      forM csvFiles $ \name -> do
        let nameAndDir = getNameAndDir name out prefixFileName (snd suffisRegExps)
        isExist <- System.Directory.doesFileExist $ fst nameAndDir
        if not isExist
          then do
            putStrLn name
            Network.FTP.Client.downloadbinary hftp name
            putStrLn (fst nameAndDir)
            copyCSVSingle tempDir name (fst nameAndDir) (snd nameAndDir)
          else return ()
      searchCSV hftp xs prefixFileName suffisRegExps out tempDir

    copyCSVSingle tempDir fileName csvFileName localDir= do
      putStrLn csvFileName
      System.Directory.createDirectoryIfMissing True localDir
      System.Directory.copyFile (tempDir </> fileName) csvFileName
      return ()

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
      Network.FTP.Client.login hftp userName (Just password) (Just account)
      return hftp

