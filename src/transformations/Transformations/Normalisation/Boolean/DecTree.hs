module Transformations.Normalisation.Boolean.DecTree
    ( Atom
    , Element (..)
    , DecTree (..)
    , BooleanOp (..)
    , singleNode
    , combineTrees
    , negateTree
    , transformAtoms
    , normaliseTree
    ) where

import Language.Haskell.Utility.PrettyPrint
import Language.Haskell.Generated.Syntax

import Control.Arrow hiding (left, right)
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe

type Atom = Expr

-- | The elements within a boolean predicate that are not themselves applications of boolean
--   operators. Equality and comparison expressions are noted explicitly, while all other
--   expressions are considered 'atoms' that will not be further normalised during this step.
data Element
    = EqualRelation Atom Atom
    | LessThanRelation Atom Atom
    | Atom Atom
    deriving (Eq, Ord, Show)

-- | The name at each level of this tree should correspond to the names-record within the BooleanAtom
--   that houses this decision tree. They should be ordered in the same manner as well. When a name is
--   assigned to be true, go to the left; go right in case it is assigned false. After traversing the
--   tree with a certain assignment to all names, you will arrive at the truth value of the entire
--   expression given this assignment.
--   The tree may stop before all names are seen in case all assignments from that point are either
--   true or false.
data DecTree
    = DLeaf Bool
    | DNode DecTree Element DecTree
    deriving (Eq, Ord, Show)

-- | Pretty-print a decision tree.
instance PrettyPrint DecTree where
    pprint = printTree' 0
      where
        printTree' d (DNode l x r) =
            concat
                [ replicate d '\t'
                , "|"
                , printEl x
                , "|\n"
                , printTree' (d + 1) l
                , printTree' (d + 1) r
                ]
        printTree' d (DLeaf b) = concat [replicate d '\t', "|", if b then "T" else "F", "|\n"]

        printEl (EqualRelation a b) = concat [pprint a, " = ", pprint b]
        printEl (LessThanRelation a b) = concat [pprint a, " < ", pprint b]
        printEl (Atom a) = pprint a

-- | A decision tree containing a single element.
singleNode :: Element -> DecTree
singleNode x = DNode (DLeaf True) x (DLeaf False)

-- | Represents a binary boolean operator.
data BooleanOp = BAnd | BOr

-- | Combine two decision trees with a particular operator.
--   If both argument trees are lexicographically ordered, the result is as well.
combineTrees :: BooleanOp -> DecTree -> DecTree -> DecTree
combineTrees op ta tb =
    case (ta, tb) of
        (DLeaf x, _) -> case op of
            BAnd -> if x then tb else DLeaf False
            BOr -> if x then DLeaf True else tb
        (_, DLeaf _) -> combineTrees op tb ta
        (DNode la na ra, DNode lb nb rb) ->
            case compare na nb of
                EQ -> DNode (combineTrees op la lb) na (combineTrees op ra rb)
                LT -> DNode (combineTrees op la tb) na (combineTrees op ra tb)
                GT -> DNode (combineTrees op ta lb) nb (combineTrees op ta rb)

-- | Flip the leaf values of a decision tree.
negateTree :: DecTree -> DecTree
negateTree (DLeaf x) = DLeaf $ not x
negateTree (DNode l x r) = DNode (negateTree l) x (negateTree r)

-- | Replace subtrees that always result into True or False with leaves.

{-trimTree :: DecTree -> DecTree-}
{-trimTree (DNode l x r) = case (trimTree l, trimTree r) of-}
{-(DLeaf a, DLeaf b) | a == b -> DLeaf a-}
{-(l',r') -> DNode l' x r'-}
{-trimTree leaf = leaf-}

-- | Apply a transformation on the atoms within the nodes of a decision tree.
transformAtoms :: (Atom -> Atom) -> DecTree -> DecTree
transformAtoms _ (DLeaf l) = DLeaf l
transformAtoms f (DNode l a r) = DNode (transformAtoms f l) (elMap f a) (transformAtoms f r)
  where
    elMap f' x = case x of
        Atom a' -> Atom $ f' a'
        EqualRelation a' b -> EqualRelation (f' a') (f' b)
        LessThanRelation a' b -> LessThanRelation (f' a') (f' b)

-- | Transforms an arbitrary decision tree into a lexicographically ordered one. I.e. provide an
--   equivalent tree for which each element at some depth i is lexicographically smaller than the
--   elements at depth i - 1.
--
--   In the used ordening, equality expression precede comparisons, which in turn precede  single
--   atoms.
orderTree :: DecTree -> DecTree
orderTree (DLeaf b) = DLeaf b
orderTree (DNode l el r) =
    let el' = DNode (DLeaf True) el (DLeaf False)
        notEl' = DNode (DLeaf False) el (DLeaf True)
        (l', r') = (orderTree l, orderTree r)
    in  combineTrees
            BOr
            (combineTrees BAnd el' l')
            (combineTrees BAnd notEl' r')

--------------------------
-- Tree normalisation.
--------------------------

-- | An environment contains a mapping from each atom to its lexicographically smallest equal, if
--   any. Furthermore, it is maintained which of these lexicographical minima are explicitly not
--   equal to each other.
data Env = Env
    { toMin :: Map Atom Atom
    , fromMin :: Map Atom [Atom]
    , disjunctMins :: Map Atom [Atom]
    }

{-printEnv (Env t f d) = concat $ intersperse "\n!" $ map (drop 9) [show t, show f, show d]-}

withToMin :: (Map Atom Atom -> Map Atom Atom) -> Env -> Env
withToMin f env = env{toMin = f $ toMin env}

withFromMin :: (Map Atom [Atom] -> Map Atom [Atom]) -> Env -> Env
withFromMin f env = env{fromMin = f $ fromMin env}

withDisjunctMins :: (Map Atom [Atom] -> Map Atom [Atom]) -> Env -> Env
withDisjunctMins f env = env{disjunctMins = f $ disjunctMins env}

-- | The empty environment.
emptyEnv :: Env
emptyEnv = Env M.empty M.empty M.empty

-- | Gets the lexicographically smallest atom equal to the first argument, according to an
--   environment.
getMinimalEqual :: Atom -> Env -> Atom
getMinimalEqual a = M.findWithDefault a a . toMin

-- | Given two equal atoms, update the environment with this equality. If this equality results in
--   a contradition, Nothing is returned.
updateEnvEqual :: Atom -> Atom -> Env -> Maybe Env
updateEnvEqual a b env = do
    let minA = getMinimalEqual a env
        minB = getMinimalEqual b env

    -- Check for contradictions.
    guard $ not $ minB `elem` M.findWithDefault [] minA (disjunctMins env)

    -- Unify the smallest and largest of the two minimima.
    let newMin = min minA minB
    let oldMin = max minA minB
    return $ replaceMinimum oldMin newMin env
  where
    -- Replace one minimal atom with another.
    replaceMinimum :: Atom -> Atom -> Env -> Env
    replaceMinimum oldMin newMin env'
        | oldMin == newMin = env'
        | otherwise =
            let oldDisjunct = M.findWithDefault [] oldMin $ disjunctMins env'
                newDisjunct = M.findWithDefault [] newMin $ disjunctMins env'
                oldFromMin = (oldMin :) $ M.findWithDefault [] oldMin $ fromMin env'
                newFromMin = M.findWithDefault [] newMin $ fromMin env'
            in  withDisjunctMins (M.delete oldMin)
                    >>> withFromMin (M.delete oldMin)
                    >>> withDisjunctMins (M.insert newMin (union oldDisjunct newDisjunct))
                    >>> withFromMin (M.insert newMin (union oldFromMin newFromMin))
                    >>> foldr (>>>) id [withToMin (M.insert a' newMin) | a' <- oldFromMin]
                    $ env'

-- | Given two non-equal atoms, update the environment with this inequality. If this results in a
--   contradition, Nothing is returned.
updateEnvNotEqual :: Atom -> Atom -> Env -> Maybe Env
updateEnvNotEqual a b env = do
    guard (a /= b)
    let minA = getMinimalEqual a env
    let minB = getMinimalEqual b env
    guard (minA /= minB)
    return $
        withDisjunctMins (M.alter (alterCons minB) minA) $
            withDisjunctMins (M.alter (alterCons minA) minB) $
                env
  where
    alterCons x mxs = Just $ union [x] $ fromMaybe [] mxs

-- | Performs a tree normalisation step. It may change elements within the tree to
--   lexicographically smaller ones and prune branches of the tree. Therefore, it holds for any
--   DecTrree t that (normaliseTreeStep t <= t).
normaliseTreeStep :: DecTree -> DecTree
normaliseTreeStep = normaliseTreeStep' emptyEnv
  where
    normaliseTreeStep' _ (DLeaf b) = DLeaf b
    normaliseTreeStep' env (DNode l el r) =
        let (envL, el', envR) =
                ( case el of
                    Atom a -> (Just env, Atom $ getMinimalEqual a env, Just env)
                    LessThanRelation a b ->
                        let minA = getMinimalEqual a env
                            minB = getMinimalEqual b env
                        in  (Just env, LessThanRelation minA minB, Just env)
                    EqualRelation a b ->
                        let minA = getMinimalEqual a env
                            minB = getMinimalEqual b env
                            left = updateEnvEqual a b env
                            right = updateEnvNotEqual a b env
                        in  (left, EqualRelation (min minA minB) (max minA minB), right)
                )
        in  case (fmap (flip normaliseTreeStep' l) envL, fmap (flip normaliseTreeStep' r) envR) of
                (Nothing, Nothing) -> DLeaf False
                (Just l', Nothing) -> l'
                (Nothing, Just r') -> r'
                (Just (DLeaf a), Just (DLeaf b)) | a == b -> DLeaf a
                (Just l', Just r') -> DNode l' el' r'

-- | Performs tree normalisation by applying normaliseTreeStep and then reordening the tree. This
--   is repeated until a fixed point is reached. Because of the property (normaliseTreeStep t <= t)
--   for each DecTree t, this can be done in a finite amount of steps.
normaliseTree :: DecTree -> DecTree
normaliseTree = findFix . iterate (normaliseTreeStep . orderTree) . normaliseTreeStep
  where
    findFix (x1 : x2 : xs)
        | x1 == x2 = x1
        | otherwise = findFix $ x2 : xs
    findFix _ = error "DecTree: findFix failure!"
