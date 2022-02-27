{- Copyright Andrew Butterfield 2017-2020 -}
module Main where

import Data.List
import Data.Maybe

import System.Environment
import System.Directory
import System.FilePath
import Control.Exception

import Utilities
import REPL
import AST
import Matching
import HParse
import Theory
import Check

version = "0.9.3.0"

main :: IO ()
main
 = do args <- getArgs
      case args of
        [] ->  repl
        [nm]  -> batch nm
        _ -> putStrLn
               $ unlines [ "usage: prfchk [name]"
                         , " name is of a theory file"
                         , " (default extension '.thr')"
                         , "If no name given, the command line interface runs"
                         , "If name is given,"
                         , " that theory is loaded and all theorems are checked"
                         ]

batch :: String -> IO ()
batch nm
  = do putStrLn ("\n\t****\n\tRunning Proof Check v"++version)
       hreqs <- loadTheory [nm] hreqs0
       case currThry hreqs of
         Nothing    -> putStrLn ( "Failed to load theories/"++nm++".thr")
         Just thry
           -> do let hms = hmods hreqs
                 let hth = hthrys hreqs ++ [thry]
                 putStrLn "STARTING BATCH CHECK...\n"
                 sequence_ $ map (showReport  . checkTheorem hms hth) $ thTheorems thry
                 putStrLn "\nFINISHED BATCH CHECK"

repl :: IO ()
repl
  = do runREPL hreqWelcome hreqConfig hreqs0
       return ()

hreqWelcome = unlines
 [ "Welcome to Proof Check v"++version
 , "To run in batch mode, give name of theory file when invoking from shell."
 , "Type '?' for help."
 ]

hreqConfig
  = REPLC
      hreqPrompt
      hreqEOFreplacmement
      hreqParser
      hreqQuitCmds
      hreqQuit
      hreqHelpCmds
      hreqCommands
      hreqEndCondition
      hreqEndTidy


data HReqState
  = HReq { hmods :: [Mdl]
         , hthrys :: [Theory]
         , currThry :: Maybe Theory
         }
  deriving Show

hmods__ f hrs = hrs{ hmods = f $ hmods hrs} ; hmods_ h = hmods__ $ const h
hthrys__ f hrs = hrs{ hthrys = f $ hthrys hrs} ; hthrys_ h = hthrys__ $ const h
currThry__ f hrs = hrs{ currThry = f $ currThry hrs}
currThry_ h = currThry__ $ const h

hreqs0 = HReq [] [] Nothing

type HReqCmd       =  REPLCmd      HReqState
type HReqCmdDescr  =  REPLCmdDescr HReqState
type HReqExit      =  REPLExit     HReqState
type HReqCommands  =  REPLCommands HReqState
type HReqConfig    =  REPLConfig   HReqState

hreqPrompt :: Bool -> HReqState -> String
hreqPrompt _ _ = "prfchk> "

hreqEOFreplacmement = [nquit]

hreqParser = wordParse

hreqQuitCmds = [nquit] ; nquit = "q"

hreqQuit :: HReqExit
hreqQuit _ hreqs = putStrLn "\nGoodbye!\n" >> return (True, hreqs)

hreqHelpCmds = ["?"]


-- we don't use these features in the top-level REPL
hreqEndCondition _ = False
hreqEndTidy _ hreqs = return hreqs

hreqCommands :: HReqCommands
hreqCommands = [ cmdShowState
               -- , showTheoryFiles
               , cmdShowLaws
               -- , cmdLoadHaskell -- deprecated for now.
               , cmdLoadTheory
               , cmdCheckTheorem
               , cmdParseHaskell
               ]

cmdShowState :: HReqCmdDescr
cmdShowState
  = ( "state"
    , "show state"
    , "show short summary of state contents"
    , showState )

showState _ hreqs
  = do showHModNames   $ hmods    hreqs
       showTheoryNames $ hthrys   hreqs
       showCurrThry    $ currThry hreqs
       return hreqs

showHModNames [] = putStrLn "No Haskell Modules"
showHModNames hms = putStrLn ("Haskell Modules: " ++ shlist (map mname hms))

showTheoryNames [] = putStrLn "No Required Theories"
showTheoryNames thrys
  = putStrLn ("Required Theories: "++ shlist (map theoryName thrys))

showCurrThry Nothing = putStrLn "\nNo Current Theory"
showCurrThry (Just thry) = putStrLn ("\nCurrent Theory: "++theoryName thry)

shlist strs = intercalate ", " strs

showTheoryFiles :: HReqCmdDescr
showTheoryFiles
  = ( "tf"
    , "show theory files"
    , "show list of *.thr in /theories."
    , showTFiles )

showTFiles _ hreq
  = do listing <- getDirectoryContents "./theories"
       let thrFiles = filter isThr listing
       putStrLn $ unlines thrFiles
       return hreq

isThr fp = takeExtension fp == ".thr"

cmdShowLaws :: HReqCmdDescr
cmdShowLaws
  = ( "laws"
    , "'law' names"
    , "show all law and definition names"
    , showLaws )

showLaws _ hreqs
  = do sequence_ $ map showHModLaws $ hmods hreqs
       putStrLn ""
       sequence_ $ map showTheoryLaws $ hthrys hreqs
       putStrLn ""
       case currThry hreqs of
         Nothing    -> putStrLn "No Current Theory"
         Just thry  -> do showTheoryLaws thry
                          showTheorems thry
       return hreqs

showHModLaws hmod
 = do putStrLn ("Laws in Haskell source '"++mname hmod++"'")
      sequence_ $ map showDecl $ topdecls hmod

showDecl (Fun []) = putStrLn "  !dud function definition!"
showDecl (Fun (m:_))  =  putStrLn ("  " ++ fname m)
showDecl (Bind (Var n) _ _) = putStrLn ("  " ++ n)
showDecl _ = putStrLn "  ??"

showTheoryLaws thry
  = do putStrLn ("Laws in Theory '"++theoryName thry++"'")
       sequence_ $ map showLaw $ thLaws thry

showLaw law = putStrLn ("  "++ lawName law)

showTheorems thry
  = do putStrLn ("Theorems in Theory '"++theoryName thry++"'")
       sequence_ $ map showTheorem $ thTheorems thry

showTheorem thrm = putStrLn ("  "++ thmName thrm)

-- deprecated for now
cmdLoadHaskell :: HReqCmdDescr
cmdLoadHaskell
  = ( "lh"
    , "load Haskell source"
    , unlines
        [ "lh <fname>  -- parse and dump AST for theories/<fname>.hs"
        ]
    , loadSource )

loadSource [] hreqs = putStrLn "no file given" >> return hreqs
loadSource (fnroot:_) hreqs
  = do  mdl <- readHaskell fnroot
        putStrLn "Module AST:\n"
        let aststr = show mdl
        putStrLn aststr
        writeFile (fnroot++".ast") aststr
        -- return $ hmods__ (++[mdl]) hreqs
        return hreqs

readHaskell fnroot
  = do let fname = fnroot ++ ".hs"
       modstr <- readFile fname
       parseHModule fname modstr

cmdParseHaskell :: HReqCmdDescr
cmdParseHaskell
  = ( "ph"
    , "parse Haskell"
    , "ph <haskell-expr> -- parse haskell expression on command line"
    , parseHaskell )

parseHaskell args hreqs
 = do case hParseE (ParseMode "ph") [] [(1,estr)] of
        But msgs -> putStrLn $ unlines msgs
        Yes (hsexp,_)
          -> do putStrLn "haskell-src parse:"
                putStrLn $ show hsexp
                let expr = hsExp2Expr preludeFixTab hsexp
                putStrLn "simple AST version:"
                putStrLn $ show expr
      return hreqs
 where estr = unwords args


cmdLoadTheory :: HReqCmdDescr
cmdLoadTheory
  = ( "load"
    , "load Theory source"
    , unlines
        [ "load <fngiven>  -- load <fngiven>.thr"
        , "load <fnname>.<ext>  -- load <fname>.<ext>"
        , " -- also loads all haskell modules and theories that it imports"
        ]
    , loadTheory )

loadTheory [] hreqs = putStrLn "no file given" >> return hreqs
loadTheory (fngiven:_) hreqs
  = do res <- readTheory fngiven
       case res of
         Nothing -> return hreqs
         Just theory
           -> do putStrLn ("\nLoaded Theory '"++fngiven++"'")
                 loadDependencies theory hreqs



readTheory fngiven
  = do let fname = fixTheoryName fngiven
       thrystr <- readFile fname
       case parseTheory (ParseMode fname) thrystr of
         But msgs  ->  do putStrLn $ unlines msgs
                          return Nothing
         Yes thry  ->  return $ Just thry

fixTheoryName givenFN
  | root == givenFN  =  root ++ ".thr"
  | otherwise        =  givenFN
  where
    (root,ext) = break (=='.') givenFN

loadDependencies theory hreqs
  = do hms <- loadModDeps $ hkImports theory
       ths <- loadThryDeps $ thImports theory
       putStrLn "Theory dependencies loaded.\n"
       return $ currThry_ (Just theory)
              $ hthrys_ ths
              $ hmods_ hms
              $ hreqs

loadModDeps []  = return []
loadModDeps (n:ns)
  = do m <- readHaskell n
       ms <- loadModDeps ns
       return (m:ms)

loadThryDeps [] = return []
loadThryDeps (t:ts)
  = do res <- readTheory t
       case res of
         Nothing  -> loadThryDeps ts
         Just thry -> do thrys <- loadThryDeps ts
                         return (thry:thrys)

cmdCheckTheorem :: HReqCmdDescr
cmdCheckTheorem
  = ( "check"
    , "check theorem"
    , "check <name> -- check theorem called name"
    , theoremCheck )

theoremCheck [] hreqs
  = do putStrLn "no theorem specified"
       return hreqs

theoremCheck (n:_) hreqs
  = do case currThry hreqs of
         Nothing
           ->  putStrLn "no current theory"
         Just thry
           ->  case findTheorem n $ thTheorems thry of
                 Nothing   ->  putStrLn ("Theorem not found: "++n)
                 Just thm  ->  showReport $
                     checkTheorem (hmods hreqs) (hthrys hreqs ++ [thry]) thm
       return hreqs
