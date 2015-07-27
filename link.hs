import System.Posix.Files
import System.Directory
import System.Environment
import Control.Monad
import Control.Exception

files = [("zshrc", ".zshrc")
        ,("zprofile", ".zprofile")
        ,("zaliases", ".zaliases")
        ,("emacs", ".emacs")
        ,("sshconfig", ".ssh/config")]
        
main =
  do cwd <- getCurrentDirectory
     home <- getEnv "HOME"
     forM_ files $
       \(from,to) ->
         handle (print::IOError -> IO ())
                (createSymbolicLink (cwd ++ "/" ++ from)
                                    (home ++ "/" ++ to))
