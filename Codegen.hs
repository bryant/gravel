module Codegen (cgModule) where

import Gravel
import qualified LLVM.General as L
import qualified LLVM.General.AST as L
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.Context as L
import qualified LLVM.General.AST.Constant as C
import Control.Monad.State (State(..))

data CodegenContext = CodegenContext {
    defs :: [L.Definition]
    }

type CodegenState = State CodegenContext

cgModule (Module toplevels) = L.defaultModule {
    L.moduleDefinitions = map cgTopLevel toplevels
    }

cgTopLevel (TopFuncDecl (ExternFuncDecl ty name _)) = L.GlobalDefinition $ L.functionDefaults {
    G.returnType = mapType ty,
    G.name = L.Name name,
    G.basicBlocks = []
    }
cgTopLevel (TopFuncDecl (FuncDecl ty name _ _)) = cgTopLevel extfn
    where extfn = TopFuncDecl $ ExternFuncDecl ty name []
cgTopLevel (TopVarDecl (VarDecl name _ _)) = L.GlobalDefinition $ L.globalVariableDefaults {
    G.name = L.Name name,
    G.type' = L.IntegerType 32,
    G.initializer = Nothing
    }

mapType "i32" = L.IntegerType 32
