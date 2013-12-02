import Control.Monad.Error
import LLVM.General
import LLVM.General.AST
import LLVM.General.AST.Global
import LLVM.General.Context

z = withContext $ \c -> runErrorT $ withModuleFromAST c m moduleString
    where
    m = defaultModule { moduleDefinitions = [asdfFunc] }
    asdfFunc = GlobalDefinition functionDefaults {
        returnType = IntegerType 32,
        name = Name "asdf"
        }

main = do
    Right rv <- z
    putStrLn rv
