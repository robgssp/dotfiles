#!/usr/bin/env runhaskell

import System.Posix.Files
import System.Directory
import System.Environment
import System.FilePath
import Control.Monad
import Control.Exception
import Text.Printf

files = [("zshrc", ".zshrc")
        ,("zprofile", ".zprofile")
        ,("zaliases", ".zaliases")
        ,("init.el", ".emacs")
        ,("early-init.el", ".emacs.d/early-init.el")
        ,("default.el", ".emacs.d/straight/versions/default.el")
        ,("sshconfig", ".ssh/config")]

main =
  do cwd <- getCurrentDirectory
     home <- getEnv "HOME"
     forM_ files $
       \(from,to) ->
         handle (print::IOError -> IO ())
                (ensureLink (cwd ++ "/" ++ from)
                            (home ++ "/" ++ to))

ensureLink from to = do
  exists <- doesPathExist to
  if exists then do
    symlink <- pathIsSymbolicLink to
    unless symlink $
      fail (printf "Cannot link %s: path exists" to)
    target <- getSymbolicLinkTarget to
    unless (target == from) $
      fail (printf "Cannot link %s: link already points to %s" to target)
    else do
    ensureDirsExist (takeDirectory to)
    createFileLink from to

ensureDirsExist path = do
  foldM (\root dir -> do
            let target = root </> dir
            exists <- doesPathExist target
            unless exists $
              createDirectory target
            return $ target)
    "/" (splitPath path)
  return ()
