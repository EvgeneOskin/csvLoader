import SRC.App.MyIO
import System.Environment (getArgs)
main = getArgs >>= SRC.App.MyIO.processArgs >>= SRC.App.MyIO.run