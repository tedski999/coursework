\section{Theory}
\begin{verbatim}
Copyright  Andrew Butterfield (c) 2017-2020

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{verbatim}
\begin{code}
module Theory
( Theory(..), parseTheory
, Theorem(..), findTheorem
, Law(..), InductionScheme(..)
, Strategy(..), Calculation(..)
, Justification(..), JRel(..), JLaw(..), Usage(..), Focus(..)
)
where

import Prelude hiding(fail)
import Control.Monad.Fail

import Data.Char
import Utilities
import AST
import HParse

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
mdbg msg x = return $! dbg msg x
\end{code}


\subsection{Theory Document Structure}

Typically a keyword at the start of a line introduces something.
We start with \texttt{THEORY} and zero or more imports:
\def\TOPSYNTAX{\texttt{
\\THEORY <TheoryName>
\\IMPORT-THEORY <Name>
\\IMPORT-HASKELL <Name>
}}

\TOPSYNTAX

These are followed by zero or more entries
that describe laws, induction schemes and theorems.

Laws are described by the following ``one-liner'' construct:
\def\LAWSYNTAX{\texttt{
\\LAW <name> <br?> <expr>
}}

\LAWSYNTAX

Here, \verb"<br?>" means that the following part
is either entirely on this line,
or else occupies a number of subsequent lines.
There can be a blank line before it,
and must be a blank line after it.
The following part itself must
not have blank lines embedded in it.

An induction-scheme is described by the following four lines:
\def\INDSCHEMASYNTAX{\texttt{
\\INDUCTION-SCHEME <Type>
\\BASE <value>
\\STEP <var> --> <expr>
\\INJ  <br?> <expr>  ==  <expr>
}}

\INDSCHEMASYNTAX


\newpage

A theorem has the following top-level structure:
\def\THEOREMSYNTAX{\texttt{
\\THEOREM <name>  <br?> <expr>
\\STRATEGY <strategy>
\\  <strategy-body>
\\QED <name>
}}

\THEOREMSYNTAX

Strategies include:
\def\ReduceAll{ReduceAll}
\def\ReduceLHS{ReduceLHS}
\def\ReduceRHS{ReduceRHS}
\def\ReduceBoth{ReduceBoth}
\def\STRATEGIES{\texttt{
\\\ReduceAll
\\\ReduceLHS
\\\ReduceRHS
\\\ReduceBoth
}}
\def\Induction{Induction}
\def\DOINDUCTION{\texttt{
\\\Induction <ind-var> :: <type>
}}
\def\SDOINDUCTION{\texttt{
\\STRATEGY Induction <ind-var> :: <type>
}}

\STRATEGIES
\DOINDUCTION

The choice of strategy will then determine the resulting structure:
\def\INDUCTIONSYNTAX{\texttt{
\\BASE <val> <br!> <expr>
\\<one of the other four strategies>
\\QED BASE
\\STEP <expr>
\\ASSUME <br!> <expr>
\\SHOW <br!> <expr>
\\<one of the other four strategies>
\\QED STEP
}}
\def\REDBOTHSYNTAX{\texttt{
\\LHS
\\<calculation>
\\RHS
\\<calculation>
}}
\begin{description}
  \item [ReduceAll]
    \begin{verbatim}
      <calculation>
    \end{verbatim}
  \item [ReduceLHS]
    \begin{verbatim}
      <calculation>
    \end{verbatim}
  \item [ReduceRHS]
    \begin{verbatim}
      <calculation>
    \end{verbatim}
  \item [ReduceBoth]~\\
   \REDBOTHSYNTAX
  \item [Induction]~\\
    \INDUCTIONSYNTAX
    \\Here, \verb"<br!>" is similar to \verb"<br?>",
    except that a line break at this point is mandatory.
\end{description}

A calculation is a sequence of formul\ae\ seperated by justification lines,
which always start with an equal sign. Blank lines are allowed
around justification lines.
\def\CALCSYNTAX{\texttt{
\\<expr1>
\\ = <justification1>
\\ ...
\\ = <justificationN>
\\<exprN+1>
}}

\CALCSYNTAX

\newpage
The justification format is as follows:
\lstinputlisting[basicstyle=\ttfamily]{doc/justifications.txt}



\newpage
\subsection{Datatypes}

\TOPSYNTAX \dots
\begin{code}
data Theory
 = THEORY {
     theoryName  :: String
   , thImports   :: [String]  -- Theory Names
   , hkImports   :: [String]  -- Haskell Module names
   , thLaws      :: [Law]
   , thIndScheme :: [InductionScheme]
   , thTheorems  :: [Theorem]
   }
 deriving Show

thImports__   f thry = thry{ thImports   = f $ thImports thry }
hkImports__   f thry = thry{ hkImports   = f $ hkImports thry }
thLaws__      f thry = thry{ thLaws      = f $ thLaws    thry }
thIndScheme__ f thry = thry{ thIndScheme = f $ thIndScheme thry }
thTheorems__  f thry = thry{ thTheorems  = f $ thTheorems thry }
\end{code}

\LAWSYNTAX
\begin{code}
data Law
 = LAW {
     lawName :: String
   , lawEqn :: Expr
   }
 deriving Show
\end{code}

\INDSCHEMASYNTAX
\begin{code}
data InductionScheme
 = IND {
     indType :: String
   , indVar  :: String  -- generic induction variable
   , indBase :: Expr    -- base value
   , indStep :: Expr    -- induction var to step expression
   }
 deriving Show
\end{code}

\THEOREMSYNTAX
\begin{code}
data Theorem
 = THEOREM {
     thmName :: String
   , theorem :: Expr
   , strategy :: Strategy
   }
 deriving Show
\end{code}

\STRATEGIES
\begin{code}
data Strategy
 = ReduceAll Calculation
 | ReduceLHS Calculation
 | ReduceRHS Calculation
 | ReduceBoth Calculation Calculation
\end{code}
\SDOINDUCTION
\INDUCTIONSYNTAX
\begin{code}
 | Induction { -- goal is what we are proving by induction
     iVar :: (String,String)  -- var :: type
   , baseVal :: Expr          -- base value
   , bGoal :: Expr            --  goal[baseVal/var]
   , baseStrategy :: Strategy
   , stepExpr :: Expr         -- expr
   , assume :: Expr           -- goal
   , iGoal :: Expr            -- goal[stepExpr/var]
   , stepStrategy :: Strategy
   }
 deriving Show
\end{code}

\CALCSYNTAX
\begin{code}
data Calculation
 = CALC {
     goal :: Expr
   , calcs :: [(Justification,Expr)]
   }
 deriving Show
\end{code}

\newpage
Justifications:
\lstinputlisting[basicstyle=\ttfamily]{doc/justifications.txt}
\begin{code}
data Justification
 = BECAUSE {
     jrel :: JRel
   , law :: JLaw
   , usage :: Usage
   , focus :: Focus
   }
 deriving Show
data JRel = JEq deriving (Eq, Show)
data JLaw = L String | D String Int | IH | CS | SMP deriving (Eq, Show)
data Usage = Whole | L2R | R2L deriving (Eq, Show)
data Focus = Top | At String Int deriving Eq

instance Show Focus where
  show Top = "top-level"
  show (At s i) = s ++ "." ++ show i
\end{code}

\newpage
\subsection{Parser Top-Level}



We start by adding in an ``empty'' theory as an accumulating
parameter,
breaking input into numbered lines
and starting the proper parsing.
\begin{code}
parseTheory :: (Monad m, MonadFail m) => ParseMode -> String -> m Theory
parseTheory pmode str
  = do (thry,_) <- theoryParser pmode theory0 $ zip [1..] $ lines str
       return thry

theory0 = THEORY { theoryName = "?", thImports = [], hkImports = []
                 , thLaws = [], thIndScheme = [], thTheorems = [] }
\end{code}
We start proper parsing by looking for \texttt{THEORY <TheoryName>}
on the first line:
\begin{code}
theoryParser :: (Monad m, MonadFail m) => ParseMode -> Theory -> Parser m Theory
theoryParser pmode theory lns
 = do (thryNm,lns') <- requireKeyAndName "THEORY" lns
      parseBody pmode theory{theoryName = thryNm} lns'
\end{code}

\begin{code}
parseBody :: (Monad m, MonadFail m) => ParseMode -> Theory -> Parser m Theory
parseBody pmode theory [] = return (theory, [])
parseBody pmode theory (ln@(lno,str):lns)
 -- we skip empty lines here...
 | emptyLine str  =  parseBody pmode theory lns

 -- simple one-liners
 | gotImpTheory   =  parseBody pmode (thImports__ (++[thryName]) theory) lns
 | gotImpCode     =  parseBody pmode (hkImports__ (++[codeName]) theory) lns

 -- complex parsers
 | gotIndSchema = callParser (parseIndSchema pmode theory typeName lno)     lns
 | gotLaw       = callParser (parseLaw pmode theory lwName lno lrest)       lns
 | gotTheorem   = callParser (parseTheorem pmode theory thrmName lno trest) lns

 | otherwise      =  pFail pmode lno 1 $ unlines
                      [ "unexpected line:\n"++str
                      , "expecting IMPORT-X, LAW, INDUCTION-SCHEME, THEOREM" ]
 where
   (gotImpTheory, thryName)       =  parseKeyAndName "IMPORT-THEORY"    str
   (gotImpCode,   codeName)       =  parseKeyAndName "IMPORT-HASKELL"   str
   (gotLaw, lwName, lrest)        =  parseOneLinerStart "LAW"           str
   (gotIndSchema, typeName)       =  parseKeyAndName "INDUCTION-SCHEME" str
   (gotTheorem, thrmName, trest)  =  parseOneLinerStart "THEOREM"       str

   callParser parser lns
     = do (theory',lns') <- parser lns
          parseBody pmode theory' lns'
\end{code}

\subsection{Parse Laws}

\LAWSYNTAX
\begin{code}
parseLaw :: (Monad m, MonadFail m) => ParseMode -> Theory  -> String -> Int -> String
         -> Parser m Theory
parseLaw pmode theory lwName lno rest lns
  = case parseExprChunk pmode lno rest lns of
      But msgs
        ->  pFail pmode lno 1 $ unlines msgs
      Yes (expr, lns')
        ->  return (thLaws__ (++[LAW lwName expr]) theory, lns')

parseExprChunk :: (Monad m, MonadFail m) => ParseMode -> Int -> String -> Parser m Expr
parseExprChunk pmode lno rest lns
 | emptyLine rest  =  parseExpr pmode restlns chunk
 | otherwise       =  parseExpr pmode lns     [(lno,rest)]
 where (chunk,restlns) = getChunk lns
\end{code}

\subsection{Parse Induction Schemata}

\INDSCHEMASYNTAX
\begin{code}
parseIndSchema :: (Monad m, MonadFail m) => ParseMode -> Theory -> String -> Int
               -> Parser m Theory
parseIndSchema pmode theory typeName lno (ln1:ln2:ln3:lns)
 | not gotBase  =  pFail pmode (lno+1) 1 "INDUCTION-SCHEME: missing BASE"
 | not gotStep  =  pFail pmode (lno+2) 1 "INDUCTION-SCHEME: missing STEP"
 | not gotInj   =  pFail pmode (lno+3) 1 "INDUCTION-SCHEME: missing INJ"
 | otherwise
     =  case parseEquivChunk pmode (lno+3) ln3rest lns of
         Nothing
           ->  pFail pmode lno 1 "INDUCTION-SCHEME: Injective law expected"
         Just ((e1,e2), lns')
           ->  parseBody pmode (thIndScheme__ (++[ind]) theory) lns'
 where
   (gotBase,bValue) = parseKeyAndValue pmode "BASE" $ snd ln1
   (gotStep,sVar,eStep) = parseKeyNameKeyValue pmode "STEP" "-->" $ snd ln2
   len = length "INJ"
   (ln3inj,ln3rest) = splitAt len $ snd ln3
   gotInj = ln3inj == "INJ"
   ind = IND typeName sVar bValue eStep
parseIndSchema pmode theory typeName lno _
  = pFail pmode lno 0 "INDUCTION-SCHEME: Incomplete"
\end{code}

Look for two expressions connected by `equality'.`
\begin{code}
parseEquivChunk :: (Monad m, MonadFail m) => ParseMode -> Int -> String
                -> Parser m (Expr,Expr)
parseEquivChunk pmode lno rest lns
 | emptyLine rest  =  parseEqual pmode restlns chunk
 | otherwise       =  parseEqual pmode lns     [(lno,rest)]
 where (chunk,restlns) = getChunk lns
\end{code}

\newpage
\subsection{Parse Theorems}

\THEOREMSYNTAX
\begin{code}
parseTheorem :: (Monad m, MonadFail m) => ParseMode -> Theory -> String -> Int -> String
             -> Parser m Theory
parseTheorem pmode theory thrmName lno rest lns
  = case parseExprChunk pmode lno rest lns of
      Nothing
        ->  pFail pmode lno 0 "Theorem expression expected"
      Just (goal, lns')
        -> do (strat,lns'') <- parseStrategy pmode lns'
              let thry = THEOREM thrmName goal strat
              let theory' = thTheorems__ (++[thry]) theory
              return (theory',lns'')
\end{code}

\subsubsection{Parse Strategies}

\begin{code}
parseStrategy :: (Monad m, MonadFail m) => ParseMode -> Parser m Strategy
parseStrategy pmode [] = pFail pmode maxBound 0 "STRATEGY: premature end of file"
parseStrategy pmode (ln:lns)
  | gotReduce     =  parseReduction pmode rstrat lns
  | gotInduction  =  parseInduction pmode vartyp lns
  | otherwise     =  pFail pmode (fst ln) 0 $ unlines
                       [ "Found: " ++ snd ln
                       , "when STRATEGY <strategy> expected." ]
  where
    (gotReduce,rstrat) = parseRedStratDecl $ snd ln
    (gotInduction,vartyp) = parseIndStratDecl $ snd ln
\end{code}

\STRATEGIES
\begin{code}
parseRedStratDecl str
  | stratSpec == ["STRATEGY","ReduceAll"]   =  (True,ReduceAll  udefc)
  | stratSpec == ["STRATEGY","ReduceLHS"]   =  (True,ReduceLHS  udefc)
  | stratSpec == ["STRATEGY","ReduceRHS"]   =  (True,ReduceRHS  udefc)
  | stratSpec == ["STRATEGY","ReduceBoth"]  =  (True,ReduceBoth udefc udefc)
  | otherwise  =  (False,error "not a reduction strategy")
  where
    stratSpec = words str
    udefc = error "undefined reduce calculation"
\end{code}

\newpage
\begin{code}
parseReduction :: (Monad m, MonadFail m) => ParseMode -> Strategy
               -> Parser m Strategy
\end{code}
\texttt{
\\\ReduceAll | \ReduceLHS | \ReduceRHS
\\<Calculation>
}
\begin{code}
-- single reductions end with "QED"
parseReduction pm (ReduceAll _) lns  =  parseReduction' pm "QED" ReduceAll lns
parseReduction pm (ReduceLHS _) lns  =  parseReduction' pm "QED" ReduceLHS lns
parseReduction pm (ReduceRHS _) lns  =  parseReduction' pm "QED" ReduceRHS lns
\end{code}
\texttt{
\\\ReduceBoth
\REDBOTHSYNTAX
}
\begin{code}
parseReduction pm (ReduceBoth _ _) lns
 = do (_,lns1) <- requireKey "LHS" lns
      -- first reduction ends with "RHS"
      (ReduceAll red1,lns2) <- parseReduction' pm "RHS" ReduceAll lns1
      -- second reduction ends with "QED"
      (ReduceAll red2,lns3) <- parseReduction' pm "QED" ReduceAll lns2
      return (ReduceBoth red1 red2,lns3)
\end{code}



\begin{code}
parseReduction' pmode calcStop reduce lns
 = do (calc, lns') <- parseCalculation pmode calcStop lns
      -- expect calcStop
      completeCalc pmode calcStop reduce calc lns'

completeCalc pmode calcStop _ _ [] = pFail pmode 0 0 $ unlines
                                       [ "Premature end of file"
                                       , "Expecting: "++calcStop ]
completeCalc pmode calcStop reduce calc ((num,str):lns)
 | take 1 (words str) == [calcStop]  =  return (reduce calc,lns)
 | otherwise  =  pFail pmode num 0 $ unlines
                     [ "Improper calc end: "++str
                     , "Expecting: "++calcStop ]
\end{code}

\SDOINDUCTION
\begin{code}
parseIndStratDecl str
  = case words str of
      ("STRATEGY":"Induction":indtvars)  ->  parseIndVars indtvars
      _ -> (False,error "not an induction strategy")

parseIndVars [] = (False,error "no induction variables defined.")
parseIndVars [var,"::",typ] = (True, (var,typ))
parseIndVars _ = (False, error "Expected var :: type")
\end{code}

\newpage
\INDUCTIONSYNTAX
\begin{code}
parseInduction :: (Monad m, MonadFail m) => ParseMode -> (String,String) -> Parser m Strategy
parseInduction pmode _ []
  = pFail pmode 0 0 "Induction proof: premature end-of-file"
parseInduction pmode vartyp lns
  = do (bval,lns1) <- requireKeyAndValue pmode "BASE" lns
       (bexpr,lns2) <- parseExprChunk pmode 0 [] lns1
       (bstrat,lns3) <- parseStrategy pmode lns2
       (sexpr,lns4) <- requireKeyAndValue pmode "STEP" lns3
       (_,lns5a) <- requireKey "ASSUME" lns4
       (ass,lns5) <- parseExprChunk pmode 0 [] lns5a -- FIX
       (_,lns6a) <- requireKey "SHOW" lns5
       (goal,lns6) <- parseExprChunk pmode 0 [] lns6a -- FIX
       (sstrat,lns7) <- parseStrategy pmode lns6
       (thnm,lns8) <- requireKeyAndName "QED" lns7
       return ( Induction { iVar = vartyp
                          , baseVal = bval
                          , bGoal = bexpr
                          , baseStrategy = bstrat
                          , stepExpr = sexpr
                          , assume = ass
                          , iGoal = goal
                          , stepStrategy = sstrat
                          }
              , lns8
              )
\end{code}


\CALCSYNTAX
\begin{code}
type Steps = [(Line,Lines)]
\end{code}

This requires multiple ``chunks'' to be parsed.
Blank lines are separators,
as are lines beginning with a leading space followed by a single equal sign.
A calculation is ended by a line starting with \texttt{calcStop}.
\begin{code}
parseCalculation :: (Monad m, MonadFail m) => ParseMode -> String -> Parser m Calculation
parseCalculation pmode calcStop lns
  = do (calcChunks,rest) <- takeLinesBefore calcStop lns
       ((fstChunk,sepChunks),_) <- splitLinesOn pmode isJustificationLn calcChunks
       (goalPred,_) <- parseExpr pmode [] fstChunk
       steps <- parseSteps pmode sepChunks
       return (CALC goalPred steps, rest)
\end{code}

Break line-list at the first use of a designated keyword,
discarding empty lines along the way
\begin{code}
takeLinesBefore :: (Monad m, MonadFail m) => String -> Parser m Lines
takeLinesBefore _ [] = return ( [], [] )
takeLinesBefore key lns@(ln:lns')
 | null lnwords         =  takeLinesBefore key lns'
 | head lnwords == key  =  return ( [], lns )
 | otherwise            =  do (before,after) <- takeLinesBefore key lns'
                              return ( ln:before, after )
 where lnwords = words $ snd ln
\end{code}

A justification line has a first word that is an equals-sign (for now).
\begin{code}
isJustificationLn :: Line -> Bool
isJustificationLn (_,str)  =  case words str of
                                []     ->  False
                                (w:_)  ->  w `elem` ["="]
\end{code}

\newpage
Split into maximal chunks seperated by lines that satisfy \texttt{splitHere}:
\begin{code}
splitLinesOn :: (Monad m, MonadFail m)
             => ParseMode -> (Line -> Bool) -> Parser m (Lines,Steps)

-- we expect at least one line before split
splitLinesOn pmode splitHere [] = pFail pmode 0 0 "premature end of calc."
splitLinesOn pmode splitHere (ln:lns)
 | splitHere ln  = pFail pmode (fst ln) 0 $ unlines
                     [ "Cannot start with: " ++ snd ln
                     , "Expecting expression" ]
 | otherwise  =  splitLinesOn' pmode splitHere [ln] lns

-- seen initial chunk, looking for first split
splitLinesOn' pmode splitHere knuhc []  =  return ((reverse knuhc,[]),[])
splitLinesOn' pmode splitHere knuhc (ln:lns)
 | splitHere ln  =  splitLinesOn'' pmode splitHere (reverse knuhc) [] ln [] lns
 | otherwise  = splitLinesOn' pmode splitHere (ln:knuhc) lns

-- found split
-- accumulating post-split chunk
splitLinesOn'' pmode splitHere chunk0 spets split knuhc []
 | null knuhc  =  pFail pmode (fst split) 0 "premature end of calc."
 | otherwise  =  return ( ( chunk0
                          , reverse ((split, reverse knuhc):spets) )
                        , [] )
splitLinesOn'' pmode splitHere chunk0 spets split knuhc (ln:lns)
 | splitHere ln  =  splitLinesOn'' pmode splitHere
                                 chunk0 ((split, reverse knuhc):spets) ln [] lns
 | otherwise  = splitLinesOn'' pmode splitHere chunk0 spets split (ln:knuhc) lns
\end{code}

Parsing calculation steps:
\begin{code}
parseSteps :: (Monad m, MonadFail m) => ParseMode -> Steps -> m [(Justification,Expr)]
parseSteps pmode [] = return []
parseSteps pmode ((justify,chunk):rest)
  = do just <- parseJustification pmode justify
       (exp,_) <- parseExpr pmode [] chunk
       steps <- parseSteps pmode rest
       return ((just,exp):steps)
\end{code}

\newpage
Parsing a justification.

\lstinputlisting[basicstyle=\ttfamily]{doc/justifications.txt}

Parsing of whole line --- need at least two words
\begin{code}
parseJustification :: (Monad m, MonadFail m) => ParseMode -> Line -> m Justification
parseJustification pmode (lno,str)
 = case words str of
    (w1:w2:wrest) ->  do jr <- parseJRel w1
                         parseJustify pmode lno jr wrest w2
    _ ->  pFail pmode lno 0 "incomplete justification"
 where
    parseJRel "="  =  return JEq
    parseJRel  x   =  pFail pmode lno 1 ("unrecognised proof relation: "++x)
\end{code}

Parsing given at least two words, the first of which is OK.
If we get a succesful parse, we ignore anything leftover.
\begin{code}
parseJustify :: (Monad m, MonadFail m) => ParseMode -> Int -> JRel -> [String] -> String
             -> m Justification
parseJustify pmode lno jr wrest w2
 | w2 == "LAW"    = parseLawName pmode lno jr     wrest
 | w2 == "DEF"    = parseDef     pmode lno jr     wrest
 | w2 == "INDHYP" = parseUsage   pmode lno jr IH  wrest
 | w2 == "CASE"   = parseUsage   pmode lno jr CS  wrest
 | w2 == "SIMP"   = parseUsage   pmode lno jr SMP wrest
 | otherwise      = pFail        pmode lno  1
                                     ("unrecognised law specification: "++w2)
\end{code}


Seen a \texttt{LAW}, expecting a \texttt{fname}
\begin{code}
parseLawName pmode lno jr []         =  pFail pmode lno 0 "LAW missing name"
parseLawName pmode lno jr (w:wrest)  =  parseUsage pmode lno jr (L w) wrest
\end{code}

\newpage
Seen a \texttt{DEF}, expecting a \texttt{fname[.i]}
\begin{code}
parseDef pmode lno jr [] = pFail pmode lno 0 "DEF missing name"
parseDef pmode lno jr (w:wrest) =  parseUsage pmode lno jr (mkD w) wrest

mkD w -- any error in '.loc' results in value 0
  | nm == w          =  D nm 1
  | null dotloc      =  D w 0
  | null loc         =  D nm 0
  | all isDigit loc  =  D nm $ read loc
  | otherwise        =  D nm 0
  where
    (nm,dotloc) = break (=='.') w
    loc = tail dotloc
\end{code}

Seen law, looking for optional usage.
\begin{code}
parseUsage pmode lno jr jlaw []
                     =  return $ BECAUSE jr jlaw (defUsage jlaw) (defFocus jlaw)
parseUsage pmode lno jr jlaw ws@(w:wrest)
  | w == "l2r"  =  parseFocus pmode lno jr jlaw L2R              wrest
  | w == "r2l"  =  parseFocus pmode lno jr jlaw R2L              wrest
  | otherwise   =  parseFocus pmode lno jr jlaw (defUsage jlaw) ws

defUsage (D _ _)  =  L2R
defUsage _        =  Whole
\end{code}

Seen law and possible usage, looking for optional focus.
Expecting either \texttt{@ name} or \texttt{@ name i}
\begin{code}
parseFocus pmode lno jr jlaw u []
                                 =  return $ BECAUSE jr jlaw u $ defFocus jlaw
parseFocus pmode lno jr jlaw u [w1,w2]
  | w1 == "@"                    =  return $ BECAUSE jr jlaw u $ At w2 1
parseFocus pmode lno jr jlaw u [w1,w2,w3]
  | w1 == "@" && all isDigit w3  =  return $ BECAUSE jr jlaw u $ At w2 $ read w3
parseFocus pmode lno jr jlaw u ws
     =  pFail pmode lno 0 ("invalid focus: "++unwords ws)

defFocus (D n _)  =  At n 1
defFocus _        =  Top
\end{code}

\newpage
\subsection{``One-Liner'' Parsing}

\subsubsection{Speculative line-parses}

The following line parsers check to see if a line has a particular form,
returning a true boolean value that is so,
plus extra information if required.


\begin{code}
emptyLine :: String -> Bool
emptyLine str = all isSpace str || take 2 (dropWhile isSpace str) == "--"
\end{code}

We return a boolean that is true if the parse suceeds.
\begin{code}
parseKeyAndName :: String -> String -> (Bool, String)
parseKeyAndName key str
  = case words str of
      [w1,w2] | w1 == key  ->  (True,  w2)
      _                    ->  (False, error ("Expecting '"++key++"' and name"))
\end{code}

\begin{code}
parseKeyAndValue :: ParseMode -> String -> String -> (Bool, Expr)
parseKeyAndValue pmode key str
  = case words str of
      (w1:wrest) | w1 == key
        -> case parseExpr pmode [] [(0,unwords wrest)] of
            Nothing -> (False, error ("Bad value: "++ unwords wrest))
            Just (hsexp,_) ->  (True,  hsexp)
      _                    ->  (False, error ("Expecting '"++key++"' and value"))
\end{code}

\begin{code}
parseKeyNameKeyValue :: ParseMode -> String -> String -> String
                     -> (Bool,String,Expr)
parseKeyNameKeyValue pmode key1 key2 str
  = case words str of
      (w1:w2:w3:wrest) | w1 == key1 && w3 == key2
        -> case parseExpr pmode [] [(0,unwords wrest)] of
            Nothing -> (False, "", error ("Bad value: "++ unwords wrest))
            Just (hsexp,_) ->  (True,  w2, hsexp)
      _                    ->  (False, "", error ("Expecting '"++key2++"' and value"))
\end{code}

\begin{code}
parseOneLinerStart :: String -> String -> (Bool,String,String)
parseOneLinerStart key str
  = case words str of
      (w1:w2:rest) | w1 == key  ->  (True,  w2, unwords rest)
      _                         ->  ( False
                                    , error "parseOneLinerStart failed!"
                                    , str)
\end{code}

\newpage
\subsubsection{Mandatory one-liners}

These parsers expect a specific form of line
as the first non-empty line in the current list of lines,
and fail with an error if not found.

\begin{code}
requireKey :: (Monad m, MonadFail m) => String -> Parser m ()
requireKey key [] = fail ("EOF while expecting key "++key)
requireKey key (ln@(lno,str):lns)
 | emptyLine str  = requireKey key lns
 | otherwise
    = case words str of
        [w1] | w1 == key  ->  return ((),lns)
        _  ->  lFail lno ("Expecting '"++key++"', found:\n"++str)
\end{code}

Here, we expect something on the current line.
\begin{code}
requireKeyAndName :: (Monad m, MonadFail m) => String -> Parser m String
requireKeyAndName key [] = fail ("EOF while expecting "++key++" <name>")
requireKeyAndName key (ln@(lno,str):lns)
  | emptyLine str  =  requireKeyAndName key lns
  | otherwise
    = case words str of
        [w1,w2] | w1 == key  ->  return (w2,lns)
        _                    ->  lFail lno ("Expecting '"++key++"' and name")
\end{code}

Here we will pass over empty lines.

\begin{code}
requireKeyAndValue :: (Monad m, MonadFail m) => ParseMode -> String -> Parser m Expr
requireKeyAndValue pmode key [] = fail ("EOF while expecting "++key++" <expr>")
requireKeyAndValue pmode key (ln@(lno,str):lns)
  | emptyLine str  =  requireKeyAndValue pmode key lns
  | otherwise
    = case words str of
        (w1:wrest) | w1 == key
           ->  parseExpr pmode lns [(0,unwords wrest)]
        _  ->  fail ("Expecting '"++key++"' and expr")
\end{code}


\begin{code}
lFail lno msg = fail ("Line:"++show lno++"\n"++msg)
\end{code}


\newpage
\subsection{Chunk Parser}

A chunk is found by skipping over zero or more empty lines,
to find a maximal run of one or more non-empty lines.
A chunck is either followed by at least one empty line,
or the end of all of the lines.
\begin{code}
getChunk []       =  ([],[])

getChunk (ln@(_,str):lns)
 | emptyLine str  =  getChunk       lns
 | otherwise      =  getChunk' [ln] lns

getChunk' snl []  =  (reverse snl, [])

getChunk' snl (ln@(_,str):lns)
 | emptyLine str  =  (reverse snl,lns)
 | otherwise      =  getChunk' (ln:snl) lns
\end{code}

\newpage
\subsection{Theorem Utilities}

\begin{code}
findTheorem :: (Monad m, MonadFail m) => String -> [Theorem] -> m Theorem
findTheorem _ [] = fail "theorem not found"
findTheorem nm (thm:thms)
 | nm == thmName thm  =  return thm
 | otherwise          =  findTheorem nm thms
\end{code}
