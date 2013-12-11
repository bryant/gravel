import Control.Monad.Error
import LLVM.General
import LLVM.General.AST
import LLVM.General.AST.Global
import qualified LLVM.General.AST.Constant as C
import LLVM.General.Context

z = withContext $ \c -> runErrorT $ withModuleFromAST c m moduleString
    where
    m = defaultModule { moduleDefinitions = [asdfFunc] }
    asdfFunc = GlobalDefinition functionDefaults {
        returnType = IntegerType 32,
        name = Name "asdf",
        basicBlocks = [
            BasicBlock (UnName 0) [
                Name "multiplied" := Mul False False (ConstantOperand $ C.Int 32 32) (ConstantOperand $ C.Int 32 56) []
                ] (Do $ Ret (Just . LocalReference $ Name "multiplied") [])
            ]
        }

main = do
    Right rv <- z
    putStrLn rv
