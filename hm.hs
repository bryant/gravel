{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State (State, get, modify, runState)
import Control.Monad (forM)

data Expression
    = Var String
    | BoolLit Bool
    | IntLit Int
    | FuncDecl String Expression
    | FuncCall Expression Expression

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

monoType = PolyType []

type Substitution = Map.Map TVarID Type

compose :: Substitution -> Substitution -> Substitution
compose c2 c = c2 `Map.union` Map.map (apply c2) c

type InfEnv = Map.Map String PolyType

instance Types a => Types [a] where
    freeVar = foldl Set.union Set.empty . map freeVar
    apply cs = map $ apply cs

instance Types InfEnv where
    freeVar = freeVar . Map.elems
    apply cs = Map.map $ apply cs

instance Types Type where
    freeVar (FuncType arg ret) = freeVar arg `Set.union` freeVar ret
    freeVar (TypeVar tid) = Set.singleton tid
    freeVar _ = Set.empty

    apply cs (FuncType arg ret) = FuncType (apply cs arg) (apply cs ret)
    apply cs (TypeVar tid) = case Map.lookup tid cs of
        Nothing -> TypeVar tid
        Just t -> t
    apply _ fixedtype = fixedtype

instance Types PolyType where
    freeVar (PolyType bnds t) = freeVar t `Set.difference` Set.fromList bnds

    apply cs (PolyType bnds t) = PolyType bnds $ apply filtered t
        where filtered = foldl (flip Map.delete) cs bnds

mgu :: Type -> Type -> Substitution
mgu (FuncType a r) (FuncType a' r') = compose constraint constraint2
    where
    constraint = mgu a a'
    constraint2 = mgu (apply constraint r) (apply constraint r')
mgu (TypeVar n) t = Map.singleton n t
mgu t (TypeVar n) = mgu (TypeVar n) t
mgu IntType IntType = Map.empty
mgu BoolType BoolType = Map.empty
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
    return $ apply (Map.fromList $ zip bnds fresh) t

generalize :: InfEnv -> Type -> PolyType
generalize env t = PolyType bnds t
    where bnds = Set.toList $ freeVar t `Set.difference` freeVar env

infer :: InfEnv -> Expression -> Inf (Substitution, Type)
infer _ (BoolLit _) = return (Map.empty, BoolType)
infer _ (IntLit _) = return (Map.empty, IntType)
infer env (Var v) = case Map.lookup v env of
    Nothing -> return . error $ "Unknown variable reference " ++ show v
    Just t -> instantiate t >>= (return . (,) Map.empty)

infer env (FuncDecl f ret) = do
    binder <- newTypeVar
    let m' = Map.insert f (PolyType [] binder) $ Map.delete f env
    (subst, ty) <- infer m' ret
    return (subst, FuncType (apply subst binder) ty)

infer env (FuncCall p arg) = do
    (subst0, ty0) <- infer env p
    (subst1, ty1) <- infer (apply subst0 env) arg
    b <- newTypeVar
    let subst2 = mgu (apply subst1 ty0) (FuncType ty1 b)
    return (foldr compose subst0 [subst2, subst1], apply subst2 b)

runTypeInf env = flip runState 0 . infer env

testmgu =
    forM cases $ \(a, b) -> putStrLn $ "mgu test: " ++ show (mgu a b)
    where
    cases =
        [
          (TypeVar "a", IntType)
        , (TypeVar "b", FuncType IntType IntType)
        , (FuncType (TypeVar "c") (TypeVar "d"), FuncType IntType IntType)
        , (FuncType (TypeVar "e") BoolType, FuncType BoolType IntType)
        ]

testInference = do
    let ((_, impType), _) = runTypeInf env impossible
    print impType
    let env' = Map.insert "impossible" (monoType impType) env
    let ((_, addType), _) = runTypeInf env' call_add
    print addType
    where
    add_one = monoType $ FuncType IntType IntType
    call_add = FuncDecl "x" $ FuncCall (Var "x") (Var "add_one")
    env = Map.fromList [("add_one", add_one)]
    impossible = BoolLit False

main = testInference >> testmgu
