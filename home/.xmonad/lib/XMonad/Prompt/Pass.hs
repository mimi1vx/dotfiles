-----------------------------------------------------------------------------
-- | 
-- Module      :  XMonad.Prompt.Pass
-- Copyright   :  (c) 2014 Ondřej Súkup
--             :  (c) 2014 Moritz Ulrich
-- License     :  
--
-- 
-- Maintainer  :  Ondřej Súkup 
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides promt for Pass - CLI password storage
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Pass (
                          -- * Usage
                          -- $usage
                          passPrompt
                          ) where
-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Pass
--
-- Then add a keybinding for 'passPrompt':
--
-- >   , ((modMask x .|. controlMask, xK_h), passPrompt defaultXPconfig)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".
 
-- 
--

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
