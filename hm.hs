import qualified Data.Map as M
import qualified Data.Set as Set
import Control.Monad.State (State, get, modify, runState)
import Control.Monad (forM)

type TVarID = String

class Types a where
    freeVar :: a -> Set.Set TVarID
    apply :: Substitution -> a -> a

data Type
    = IntType
    | BoolType
    | FuncType Type Type
    | TypeVar TVarID
    deriving Show

data PolyType = PolyType [TVarID] Type deriving Show

type Substitution = M.Map TVarID Type

compose :: Substitution -> Substitution -> Substitution
compose c2 c = c2 `M.union` M.map (apply c2) c

data InfEnv = InfEnv (M.Map String PolyType)

instance Types a => Types [a] where
    freeVar = foldl Set.union Set.empty . map freeVar
    apply cs = map $ apply cs

instance Types InfEnv where
    freeVar (InfEnv m) = freeVar $ M.elems m
    apply cs (InfEnv m) = InfEnv . flip M.map m $ apply cs

instance Types Type where
    freeVar (FuncType arg ret) = freeVar arg `Set.union` freeVar ret
    freeVar (TypeVar tid) = Set.singleton tid
    freeVar _ = Set.empty

    apply cs (FuncType arg ret) = FuncType (apply cs arg) (apply cs ret)
    apply cs (TypeVar tid) = case M.lookup tid cs of
        Nothing -> TypeVar tid
        Just t -> t
    apply _ fixedtype = fixedtype

instance Types PolyType where
    freeVar (PolyType bnds t) = freeVar t `Set.difference` Set.fromList bnds

    apply cs (PolyType bnds t) = PolyType bnds $ apply filtered t
        where filtered = foldl (flip M.delete) cs bnds

mgu :: Type -> Type -> Substitution
mgu (FuncType a r) (FuncType a' r') = compose constraint constraint2
    where
    constraint = mgu a a'
    constraint2 = mgu (apply constraint r) (apply constraint r')
mgu (TypeVar n) t = M.singleton n t
mgu t (TypeVar n) = mgu (TypeVar n) t
mgu IntType IntType = M.empty
mgu BoolType BoolType = M.empty
mgu a b = error $ "non-unifiable " ++ show a ++ " -/- " ++ show b

data InfState = InfState { idgen :: Int }
type Inf = State Int

newTypeVar :: Inf Type
newTypeVar = do
    tid <- get
    modify (+1)
    return . TypeVar $ "a" ++ show tid

instantiate :: PolyType -> Inf Type
instantiate (PolyType bnds t) = do
    fresh <- forM bnds $ const newTypeVar
    return $ apply (M.fromList $ zip bnds fresh) t

generalize :: InfEnv -> Type -> PolyType
generalize env t = PolyType bnds t
    where bnds = Set.toList $ freeVar t `Set.difference` freeVar env

main = do
    print $ mgu (TypeVar "a") (IntType)
    print $ mgu (TypeVar "b") (FuncType IntType IntType)
    print $ mgu (FuncType (TypeVar "c") (TypeVar "d")) (FuncType IntType IntType)
    print $ mgu (FuncType (TypeVar "e") BoolType) (FuncType BoolType IntType)
