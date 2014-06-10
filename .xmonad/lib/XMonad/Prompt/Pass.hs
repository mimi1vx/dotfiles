------------------------------------------------------------------------------------
--          Pass Promp module based on:
--          http://blog.tarn-vedra.de/2014/05/xmonad-loves-password-store.html
------------------------------------------------------------------------------------    
-- Module:                    XMonad.Prompt.Pass
--
------------------------------------------------------------------------------------

module XMonad.Prompt.Pass (
        passPrompt)
  where

import System.Environment
import System.FilePath.Find
import System.FilePath.Posix

import XMonad
import XMonad.Prompt

data Pass = Pass
 
instance XPrompt Pass where
  showXPrompt       Pass = "Pass: "
  commandToComplete _ c  = c
  nextCompletion      _  = getNextCompletion
 
passPrompt :: XPConfig -> X ()
passPrompt c = do
  li <- io getPasswords
  mkXPrompt Pass c (mkComplFunFromList li) selectPassword
 
selectPassword :: String -> X ()
selectPassword s = spawn $ "pass -c " ++ s
 
getPasswords :: IO [String]
getPasswords = do
  home <- getEnv "HOME"
  let passwordStore = home </> ".password-store"
  entries <- find System.FilePath.Find.always (fileName ~~? "*.gpg") passwordStore
  return $ map (makeRelative passwordStore . dropExtension) entries 
