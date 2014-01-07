import qualified Data.Map as M
import qualified Data.Set as Set
import Control.Monad.State (State, get, modify, runState)

type TVarID = String

class Types a where
    freeVar :: a -> Set.Set TVarID
    apply :: Constraint -> a -> a

data Type
    = IntType
    | BoolType
    | FuncType Type Type
    | TypeVar TVarID
    deriving Show

data PolyType = PolyType [TVarID] Type deriving Show

type Constraint = M.Map TVarID Type

compose :: Constraint -> Constraint -> Constraint
compose c2 c = c2 `M.union` M.map (apply c2) c

instance Types a => Types [a] where
    freeVar = foldl Set.union Set.empty . map freeVar

    apply cs = map $ apply cs

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

mgu :: Type -> Type -> Constraint
mgu (FuncType a r) (FuncType a' r') = compose constraint constraint2
    where
    constraint = mgu a a'
    constraint2 = mgu (apply constraint r) (apply constraint r')
mgu (TypeVar n) t = M.singleton n t
mgu t (TypeVar n) = mgu (TypeVar n) t
mgu IntType IntType = M.empty
mgu BoolType BoolType = M.empty
mgu a b = error $ "non-unifiable " ++ show a ++ " -/- " ++ show b

main = do
    print $ mgu (TypeVar "a") (IntType)
    print $ mgu (TypeVar "b") (FuncType IntType IntType)
    print $ mgu (FuncType (TypeVar "c") (TypeVar "d")) (FuncType IntType IntType)
    print $ mgu (FuncType (TypeVar "e") BoolType) (FuncType BoolType IntType)
