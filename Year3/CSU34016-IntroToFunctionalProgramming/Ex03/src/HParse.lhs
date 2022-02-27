\section{Haskell Parser}
\begin{verbatim}
Copyright  Andrew Butterfield (c) 2017-2020

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module HParse
( Line, Lines, Parser
, parseHModule
, parseExpr, hParseE
, parseEqual
, hs42
, ParseMode(..), ParseResult(..), SrcLoc(..), pFail
)
where
import Prelude hiding(fail)

import Data.Char
import qualified Data.Map as M

import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax
import Control.Monad.Fail

import Utilities
import AST

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
mdbg msg x = return $! dbg msg x
\end{code}

\newpage
\subsection{Monadic Failure}
A polymorphic, monadic parser type:
\begin{code}
type Line = (Int,String)
type Lines = [Line]
type Parser m a  = Lines -> m (a,Lines)
\end{code}
From \texttt{Language.Haskell.Parser}%
\footnote{
 Has more components in \texttt{Language.Haskell.Exts.Parser},
 including fixity handling!
}%
:
\begin{verbatim}
data ParseMode = ParseMode {parseFilename :: String}
\end{verbatim}
A \texttt{SrcLoc}-based monadic failure:
\begin{code}
pFail :: (Monad m, MonadFail m) => ParseMode -> Int -> Int -> String -> m a
pFail pmode lno colno msg
  = fail (parseFilename pmode ++ ':':show lno++ ":"++show colno++" "++msg)
\end{code}

\subsection{Parser Top-Level}

\begin{code}
parseHModule :: (Monad m, MonadFail m) => String -> String -> m Mdl
parseHModule fname modstr
 = case parseModuleWithMode pmode modstr of
     ParseFailed loc msg -> pFail pmode (srcLine loc) (srcColumn loc) msg
     ParseOk hsmod -> return $ hsModule2Mdl hsmod
 where pmode = ParseMode fname
\end{code}

\newpage
\subsection{Parsing Expressions}

\begin{code}
parseExpr :: (Monad m, MonadFail m) => ParseMode -> Lines -> Parser m Expr
parseExpr pmode restlns chunk
 = do (hsexp,lns') <- hParseE pmode restlns chunk
      return (hsExp2Expr preludeFixTab hsexp,lns')

hParseE :: (Monad m, MonadFail m) => ParseMode -> Lines -> Parser m HsExp
hParseE pmode restlns [] = pFail pmode 0 0 "no expression!"
hParseE pmode restlns chunk@((lno,_):_)
  = case parseModuleWithMode pmode (mkNakedExprModule chunk) of
      ParseFailed _ msg  -> pFail pmode lno 1 msg
      ParseOk hsmod -> do hsexp <- getNakedExpr hsmod
                          return (hsexp, restlns)
\end{code}

\begin{code}
mkNakedExprModule [(_,str)]
  = unlines [ "module NakedExpr where"
            , "nakedExpr = "++str ]
mkNakedExprModule chunk
  = unlines ( [ "module NakedExpr where"
              , "nakedExpr = " ]
              ++ map snd chunk )
\end{code}

\begin{code}
getNakedExpr :: (Monad m, MonadFail m) => HsModule -> m HsExp
getNakedExpr
 (HsModule _ _ _ _ [ HsPatBind _ _ (HsUnGuardedRhs hsexp) [] ])
    = return hsexp
getNakedExpr _ = fail "can't find the naked expression"

hs42 = LInt 42
\end{code}


\subsection{Parsing Equivalences}

\begin{code}
parseEqual :: (Monad m, MonadFail m) => ParseMode -> Lines -> Parser m (Expr, Expr)
parseEqual pmode restlns [] = pFail pmode 0 0 "no equivalence!"
parseEqual pmode restlns chunk@((lno,_):_)
  = case parseModuleWithMode pmode (mkNakedExprModule chunk) of
      ParseFailed _ msg  -> pFail pmode lno 1 msg
      ParseOk hsmod -> return (getNakedEqual hsmod, restlns)
\end{code}

\begin{code}
getNakedEqual :: HsModule -> (Expr,Expr)
getNakedEqual
 (HsModule _ _ _ _ [ _, HsPatBind _ _ (HsUnGuardedRhs hsexp) [] ])
   = case hsexp of
       (HsInfixApp e1 (HsQVarOp (UnQual (HsSymbol "=="))) e2)
          ->  ( hsExp2Expr preludeFixTab e1
              , hsExp2Expr preludeFixTab e2)
       _               ->  (hs42,hs42)
getNakedEqual _  =   (hs42,hs42)
\end{code}
