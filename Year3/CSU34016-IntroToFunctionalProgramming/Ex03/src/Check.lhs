\section{Checking}
\begin{haskell}
Copyright  Andrew Butterfield (c) 2017-2020

LICENSE: BSD3, see file LICENSE at reasonEq root
\end{haskell}
\begin{code}
module Check
(Report, showReport, checkTheorem)
where

import AST
import Theory
import Matching

import Debug.Trace
dbg msg x = trace (msg ++ show x) x
mdbg msg x = return $! dbg msg x
\end{code}

\begin{code}
type Report = [String]

showReport rep = putStrLn $ unlines rep

rep :: String -> Report
rep str = lines str

rjoin :: Report -> Report -> Report
r1 `rjoin` r2 = r1 ++ r2
\end{code}

\begin{code}
checkTheorem :: [Mdl] -> [Theory] -> Theorem -> Report
checkTheorem mdls thrys thm
  = rep ("\nChecking theorem '"++thmName thm++"'")
     `rjoin` (checkStrategy mdls thrys dummyH (theorem thm) $ strategy thm)
\end{code}

Induction and case-based strategies require a hypothesis to be passed to
the calculation checker,
while the variations of reduction don't.
In the latter case we pass in a dummy hypothesis:
\begin{code}
dummyH = Var "??"
\end{code}

\begin{code}
checkStrategy :: [Mdl] -> [Theory] -> Expr -> Expr -> Strategy -> Report
\end{code}

\begin{code}
checkStrategy mdls thrys hyp goal (ReduceAll calc)
  = rep "\nStrategy: reduce all to True"
     `rjoin` checkFirst calc goal
     `rjoin` checkCalc mdls thrys hyp calc
     `rjoin` checkLast calc (LBool True)

checkStrategy mdls thrys hyp goal (ReduceLHS calc)
  = rep "\nStrategy: reduce LHS to RHS"
     `rjoin` checkFirst calc (lhsOf goal)
     `rjoin` checkCalc mdls thrys hyp calc
     `rjoin` checkLast calc (rhsOf goal)

checkStrategy mdls thrys hyp goal (ReduceRHS calc)
  = rep "\nStrategy: reduce RHS to LHS"
     `rjoin` checkFirst calc (rhsOf goal)
     `rjoin` checkCalc mdls thrys hyp calc
     `rjoin` checkLast calc (lhsOf goal)

checkStrategy mdls thrys hyp goal (ReduceBoth cLHS cRHS)
  = rep "\n Strategy: reduce RHS and LHS to same"
     `rjoin` checkBothStart goal cLHS cRHS
     `rjoin` rep "\nCheck LHS" `rjoin` checkCalc mdls thrys hyp cLHS
     `rjoin` rep "\nCheck RHS" `rjoin` checkCalc mdls thrys hyp cRHS
     `rjoin` checkSameLast cLHS cRHS

-- istrat must be (Induction ...)
checkStrategy mdls thrys hyp goal istrat
  = rep ( "\nStrategy: Induction in " ++ var ++ " :: "++ typ )
     `rjoin` checkIndScheme thrys goal bgoal ihypo igoal var typ
     `rjoin` rep "\nCheck Base Case..."
     `rjoin` checkStrategy mdls thrys hyp bgoal (baseStrategy istrat)
     `rjoin` rep "\nCheck Step Case..."
     `rjoin` checkStrategy mdls thrys ihypo igoal (stepStrategy istrat)
  where
    (var,typ) = iVar istrat
    bgoal = bGoal istrat
    igoal = iGoal istrat
    ihypo = assume istrat
\end{code}

We use four 2-character markers at the start of key lines
in reports:
\begin{description}
  \item [OK] Correct Proof Setup
  \item [!!] Incorrect Proof Setup
  \item [Ok] Correct Proof step
  \item [??] Incorrect Proof step
\end{description}
The first two are of major importance
relative to the second two.

\begin{code}
checkFirst :: Calculation -> Expr -> Report
checkFirst (CALC e0 _) e
  | e0 == e   =  rep "OK: correct first expression."
  | otherwise =  rep "!!: incorrect first expression."
\end{code}

\begin{code}
lastE :: Calculation -> Expr
lastE (CALC e [])     =  e
lastE (CALC _ steps)  =  snd $ last steps

checkLast :: Calculation -> Expr -> Report
checkLast calc e
  | (lastE calc) == e  =  rep "OK: correct last expression."
checkLast _ _          =  rep "!!: incorrect last expression."
\end{code}

\begin{code}
checkBothStart :: Expr -> Calculation -> Calculation -> Report
checkBothStart goal (CALC gLHS _) (CALC gRHS _)
  | goal == equal gLHS gRHS  = rep "OK: goal lhs/rhs"
  | otherwise                = rep "!!: (lhs = rhs) is not goal"
\end{code}

\begin{code}
checkSameLast :: Calculation -> Calculation -> Report
checkSameLast cLHS cRHS
 | lastE cLHS == lastE cRHS  =  rep "OK: last expressions are the same."
 | otherwise                 =  rep "!!: last expressions differ."
\end{code}

\begin{code}
equal :: Expr -> Expr -> Expr
equal e1 e2 = App (App eEq e1) e2
\end{code}

\begin{code}
lhsOf (App (App eq e1) _)
 | eq == eEq  =  e1
lhsOf e       =  e

rhsOf (App (App eq _) e2)
 | eq == eEq  =  e2
rhsOf e       =  e
\end{code}

\begin{code}
checkIndScheme thrys goal bgoal ihypo igoal var typ
  = case findTheoryInds thrys typ of
      Nothing -> rep ("No Induction scheme for "++typ)
      Just indscheme
       ->  rep ("Ind Scheme '"++typ++"' valid")
           `rjoin`
           assumeRep
           `rjoin`
           rep "checkIndScheme n.y.f.i."
      -- we also need to check: (see checkStrategy (Induction above))
         -- baseVal = indscheme.base
         -- bGoal = goal[baseVal/var]
         -- stepExpr isIso_to  indscheme.indStep
         -- iGoal = goal[stepExpr/var]
  where
    assumeRep = if ihypo == goal
                then rep "OK: Induction ASSUME matches goal"
                else rep "!!: Induction ASSUME different from goal"
\end{code}


\newpage
We keep the best until last \dots
\begin{code}
checkCalc :: [Mdl] -> [Theory] -> Expr -> Calculation -> Report
checkCalc mdls thrys hyp (CALC goal [])  =  rep "!!: no steps to check"
checkCalc mdls thrys hyp (CALC goal steps)
  = checkSteps mdls thrys hyp goal steps

checkSteps _ _ _ _ []  = rep "check complete"
checkSteps mdls thrys hyp goal ((just,goal'):steps)
  = checkStep mdls thrys hyp goal just goal'
     `rjoin` checkSteps mdls thrys hyp goal' steps
\end{code}

This is where all the heavy lifting is done:
\begin{code}
checkStep :: [Mdl] -> [Theory] -> Expr -> Expr -> Justification -> Expr
          -> Report

checkStep mdls thrys hyp goal (BECAUSE _ (D dnm i) howused what) goal'
-- need to modify this based on howused !!!!
 = case searchMods mdls dnm i of
   Nothing -> rep ("??: Can't find def. "++dnm++"."++show i)
   Just defn
    -> case findAndApplyDEFN (mdlsKnown mdls) defn goal howused what of
       Nothing -> rep $ unlines
                   [ "??: Failed to apply def. "++show dnm++"."++show i
                   , "  Defn: "++show defn
                   , "  Goal: "++show goal
                   , "  HowUsed: "++show howused
                   ]
       Just goal''
        -> if goal'' == goal'
           then rep ("Ok: use of def. "++dnm++"."++show i++" is correct.")
           else rep $ unlines
                 [ "??: use of def. "++dnm++"."++show i++" differs."
                 , "  Expected:\n"++show goal'
                 , "  Got:\n"++show goal''
                 ]

checkStep mdls thrys hyp goal (BECAUSE _ (L lnm) howused what) goal'
 = case findTheoryLaws thrys lnm of
     Nothing -> rep ("??: Can't find law "++lnm)
     Just thelaw
       -> case findAndApplyLAW (mdlsKnown mdls) thelaw goal howused what of
           Nothing -> rep ("??: Failed to apply law "++lnm++" "++show howused)
           Just goal''
             -> if goal'' == goal'
                 then rep ("Ok: use of law "++lnm++" "++show howused++" is correct.")
                 else rep $ unlines
                       [ "??: use of law "++lnm++" "++show howused++" differs."
                       , "Expected:\n"++show goal'
                       , "Got:\n"++show goal''
                       ]

checkStep mdls thrys hyp goal (BECAUSE _ SMP _ _) goal'
  | exprSIMP goal == goal'  =  rep ("Ok: use of SIMP is correct.")
  | otherwise               =  rep ("??: use of SIMP differs.")

checkStep mdls thrys hyp goal (BECAUSE _ IH howused what) goal'
 = case findAndApplyLAW (mdlsKnown mdls) (LAW "IH" hyp) goal howused what of
     Nothing -> rep ("??: Failed to apply Inductive Hypothesis " ++ howatwhat)
     Just goal''
       -> if goal'' == goal'
           then rep ("Ok: use of Inductive Hypothesis "++howatwhat++" is correct.")
           else rep ("??: use of Inductive Hypothesis "++howatwhat++" differs.")
 where howatwhat = show howused ++" @ "++show what
\end{code}

We need all names defined in imported haskell files:
\begin{code}
mdlsKnown = concat . map mdlKnown

mdlKnown mdl = getDefined $ topdecls mdl

getDefined [] = []
getDefined (Fun (m:_)        : tdcls)  = fname m : getDefined tdcls
getDefined (Bind (Var v) _ _ : tdcls)  = v       : getDefined tdcls
getDefined (_                : tdcls)  =           getDefined tdcls
\end{code}

\begin{code}
type Definition = (Expr,Expr,[Decl])

searchMods [] dnm i = Nothing
searchMods (mdl:mdls) dnm i
  = case searchDecls (topdecls mdl) dnm i of
      Nothing  ->  searchMods mdls dnm i
      jdefn    ->  jdefn
\end{code}

\begin{code}
searchDecls [] dnm i = Nothing
searchDecls (decl:decls) dnm i
  = case checkDecl dnm i decl of
      Nothing -> searchDecls decls dnm i
      jdefn -> jdefn
\end{code}

\begin{code}
checkDecl :: String -> Int -> Decl -> Maybe Definition

checkDecl dnm i (Bind v@(Var vnm) defn ldcls)
  | dnm == vnm && i < 2  =  Just (v,defn,ldcls)
  -- only do simple  v = e where ... binds for now

checkDecl dnm i (Fun [match])
  | dnm == fname match && i < 2
                         = Just (mkLHS dnm match,rhs match, ldecls match)
checkDecl dnm i (Fun matches)
  | i < 1  =  Nothing
  | i > length matches  =  Nothing
  | dnm == fname match  = Just (mkLHS dnm match,rhs match, ldecls match)
  where
    match = matches !! (i-1)
checkDecl _ _ _ = Nothing

mkLHS dnm match = mkApp (Var dnm) $ lhspat match

mkApp f [] = f
mkApp f (a:as)  = mkApp (App f a) as
\end{code}

This does an in-order traverse of the \texttt{goal} looking for
the sub-expression defined by \texttt{what}.
Once found, it will use \texttt{defn} to rewrite that sub-expression.
\begin{code}
findAndApplyDEFN :: [String] -> Definition -> Expr -> Usage -> Focus
                 -> Maybe Expr
findAndApplyDEFN knowns defn goal howused Top
  = applyDEFN knowns howused defn goal

findAndApplyDEFN knowns defn goal howused (At nm i)
  = case pathToIndicatedName goal nm i of
      Nothing -> Nothing
      Just path
       -> applyAtPathFocus (applyDEFN knowns howused defn) path goal
\end{code}

\begin{code}
applyDEFN :: [String] -> Usage -> Definition -> Expr -> Maybe Expr

applyDEFN knowns L2R (lhs,rhs,ldcls) expr
  = case eMatch knowns expr lhs of
      Nothing -> Nothing
      Just bind -> Just $ buildReplacement bind ldcls rhs

applyDEFN knowns R2L (lhs,rhs,ldcls) expr
  = case eMatch knowns expr rhs of
      Nothing -> Nothing
      Just bind -> Just $ buildReplacement bind ldcls lhs
\end{code}

\begin{code}
findTheoryLaws [] lnm = Nothing
findTheoryLaws (thry:thrys) lnm
  = case searchLaws (thLaws thry) lnm of
      Nothing  ->  findTheoryLaws thrys lnm
      jlaw     ->  jlaw

searchLaws [] lnm = Nothing
searchLaws (lw:laws) lnm
  | lawName lw == lnm  =  Just lw
  | otherwise  = searchLaws laws lnm
\end{code}

\begin{code}
findTheoryInds [] typ = Nothing
findTheoryInds (thry:thrys) typ
  = case searchInds (thIndScheme thry) typ of
      Nothing  ->  findTheoryInds thrys typ
      inds     ->  inds

searchInds [] typ = Nothing
searchInds (inds:indss) typ
  | indType inds == typ  =  Just inds
  | otherwise  = searchInds indss typ
\end{code}


This does an in-order traverse of the \texttt{goal} looking for
the sub-expression defined by \texttt{what}.
Once found, it will use \texttt{thelaw},
according to \texttt{howused},
to rewrite that sub-expression.
\begin{code}
findAndApplyLAW :: [String] -> Law -> Expr -> Usage -> Focus -> Maybe Expr

findAndApplyLAW knowns thelaw goal howused Top
 = applyLAW knowns howused (lawEqn thelaw) goal

findAndApplyLAW knowns thelaw goal howused (At nm i)
  = case pathToIndicatedName goal nm i of
      Nothing -> Nothing
      Just path
       -> applyAtPathFocus (applyLAW knowns howused $ lawEqn thelaw) path goal
\end{code}

\begin{code}
applyLAW :: [String] -> Usage -> Expr -> Expr -> Maybe Expr

applyLAW knowns Whole thelaw expr
  = case eMatch knowns expr thelaw of
      Nothing -> Nothing
      Just _ -> Just $ LBool True

applyLAW knowns L2R (Equal lhs rhs) expr
  = case eMatch knowns expr lhs of
      Nothing -> Nothing
      Just bind -> Just $ buildReplacement bind [] rhs

applyLAW knowns R2L (Equal lhs rhs) expr
  = case eMatch knowns expr rhs of
      Nothing -> Nothing
      Just bind -> Just $ buildReplacement bind [] lhs
\end{code}

\newpage
\subsubsection{Focus Handling}

Consider we are looking for the $i$th occurrence of name \texttt{f}
in an expression, and it is found embedded somewhere,
and is a function name applied to several arguments:
\texttt{.... f x y z ....}.
What we want returned is a pointer to that full application,
and not just to \texttt{f}.
However, this means that the location of \texttt{f}
can be arbitrarily deep down the lefthand branch of an \texttt{App},
as the above application will parse as $@ (@ (@~f~x)~y)~z$.
If the application has path $\rho$, then the path to the
occurrence of $f$ will be $\rho \cat \seqof{1,1,1}$.
So we can delete trailing ones to get up to the correct location in this case.
However if \texttt{f} occurs in an if-expression (say),
like \texttt{if f then x else y}, then if the if-expression has path $\rho$,
then $f$ has path $\rho\cat\seqof{1}$, but this last one needs to remain.
In effect we have to tag the indices to indicate if we are branching
through an application ($@$) or some other kind of node (e.g., $if$).


\begin{code}
-- we only care about App vs everything else right now
data ExprBranches = AppB | OtherB deriving (Eq, Show)
type Branch = (Int,ExprBranches)
type Path = [Branch] -- identify sub-expr by sequence of branch indices
findAllNameUsage :: String -> Path -> Expr -> [Path]
-- paths returned here are reversed, with deepest index first

findAllNameUsage nm currPath (App (Var v) e2)
  | nm == v  = currPath : findAllNameUsage nm ((2,AppB):currPath) e2

findAllNameUsage nm currPath (Var v) = if nm == v then [currPath] else []

findAllNameUsage nm currPath (Cons c) = if nm == c then [currPath] else []

findAllNameUsage nm currPath (App e1 e2)
  =  findAllNameUsage nm ((1,AppB):currPath) e1
  ++ findAllNameUsage nm ((2,AppB):currPath) e2

findAllNameUsage nm currPath (If e1 e2 e3)
  =  findAllNameUsage nm ((1,OtherB):currPath) e1
  ++ findAllNameUsage nm ((2,OtherB):currPath) e2
  ++ findAllNameUsage nm ((3,OtherB):currPath) e3

findAllNameUsage nm currPath (GrdExpr grds)
  = concat $ map (findGuardNameUsage nm currPath) $ zip [1..] grds

findAllNameUsage _ _ (LBool _) =  []
findAllNameUsage _ _ (LInt  _) =  []
findAllNameUsage _ _ (LChar _) =  []

findAllNameUsage nm currPath e = error ("findAllNameUsage NYIf "++show e)
\end{code}

\begin{code}
findGuardNameUsage nm currPath (i,(grd,res))
  =    findAllNameUsage nm ((1,OtherB):cp') grd
    ++ findAllNameUsage nm ((2,OtherB):cp') res
  where cp' = (i,OtherB):currPath
\end{code}

\begin{code}
getIth :: Int -> [a] -> Maybe a
getIth _ []      =  Nothing
getIth 1 (x:_)   =  Just x
getIth n (_:xs)  =  getIth (n-1) xs

replIth :: Int -> a -> [a] -> Maybe [a]
replIth _ _ []       =  Nothing
replIth 1 x' (x:xs)  =  Just (x':xs)
replIth n x' (x:xs)  =  do xs' <- replIth (n-1) x' xs
                           return (x:xs')
\end{code}

\newpage
Given an expression ($e$), a name ($n$), and an integer $i$,
locate the $i$th (inorder) ``effective occurence'' of $n$ in $e$.
By ``effective occurrence'' we mean that if the name is of an applied
function then we want the sub-expression that corresponds to the
application of that function to all its arguments.
For example, given $(h~f) + f~ x~ y + 1$,
the first effective occurrence of $f$ is just the $f$ that is the argument
to $h$,
while the second effective occurrence is the whole application $f~x~y$.

\begin{code}
pathToIndicatedName :: Expr -> String -> Int -> Maybe [Int]
pathToIndicatedName goal nm i
 = case findAllNameUsage nm [] goal of
      [] -> Nothing
      paths
         -> case getIth i paths of
             Nothing -> Nothing
             Just path -> Just $ reverse $ map fst $ dropWhile isApp1 path
  where
    isApp1 (1,AppB)  =  True
    isApp1 _         =  False
\end{code}

\begin{code}
applyAtPathFocus :: (Expr -> Maybe Expr) -> [Int] -> Expr -> Maybe Expr
applyAtPathFocus replace []    goal = replace goal
applyAtPathFocus replace (i:is) (App e1 e2)
  | i == 1  =  do e1' <- applyAtPathFocus replace is e1
                  return $ App e1' e2
  | i == 2  =  do e2' <- applyAtPathFocus replace is e2
                  return $ App e1 e2'
applyAtPathFocus replace (i:is) (If e1 e2 e3)
  | i == 1  =  do e1' <- applyAtPathFocus replace is e1
                  return $ If e1' e2 e3
  | i == 2  =  do e2' <- applyAtPathFocus replace is e2
                  return $ If e1 e2' e3
  | i == 3  =  do e3' <- applyAtPathFocus replace is e3
                  return $ If e1 e2 e3'
applyAtPathFocus replace (i:j:is) (GrdExpr eps)
  = do (e1,e2) <- getIth i eps
       if      j == 1 then do e1'   <-  applyAtPathFocus replace is e1
                              eps'  <-  replIth i (e1',e2) eps
                              return $ GrdExpr eps'
       else if j == 2 then do e2'   <-  applyAtPathFocus replace is e2
                              eps'  <-  replIth i (e1,e2') eps
                              return $ GrdExpr eps'
       else Nothing
applyAtPathFocus replace (i:is) (Let dcls e)   =  Nothing
applyAtPathFocus replace (i:is) (PApp nm es)   =  Nothing

applyAtPathFocus replace (i:is) goal = Nothing
\end{code}



Builtin-simplifier
\begin{code}
exprSIMP :: Expr -> Expr
exprSIMP (InfixApp e1 op e2)  =  applyOp op (exprSIMP e1) (exprSIMP e2)
exprSIMP (App e1 e2)          =  App (exprSIMP e1) (exprSIMP e2)
exprSIMP (If e1 e2 e3)        =  If (exprSIMP e1) (exprSIMP e2) (exprSIMP e3)
exprSIMP (GrdExpr eps)        =  GrdExpr $ map exprSIMP2 eps
exprSIMP (Let dcls e)         =  Let dcls $ exprSIMP e
exprSIMP (PApp nm es)         =  PApp nm $ map exprSIMP es
exprSIMP e                    =  e

exprSIMP2 (e1,e2)             =  (exprSIMP e1,exprSIMP e2)
\end{code}

The fun part:
\begin{code}
applyOp "+"  (LInt x) (LInt y)  =  LInt  (x+y)
applyOp "-"  (LInt x) (LInt y)  =  LInt  (x-y)
applyOp "*"  (LInt x) (LInt y)  =  LInt  (x*y)
applyOp "==" (LInt x) (LInt y)  =  LBool (x==y)
applyOp "/=" (LInt x) (LInt y)  =  LBool (x/=y)
applyOp "<"  (LInt x) (LInt y)  =  LBool (x<y)
applyOp "<=" (LInt x) (LInt y)  =  LBool (x<=y)
applyOp ">"  (LInt x) (LInt y)  =  LBool (x>y)
applyOp ">=" (LInt x) (LInt y)  =  LBool (x>=y)
applyOp op e1 e2 = InfixApp e1 op e2
\end{code}
