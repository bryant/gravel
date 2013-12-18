import Gravel (testParse)
import Codegen (cgModule)
import LLVM.General (withModuleFromAST, moduleString)
import LLVM.General.Context (withContext)
import Control.Monad.Error (runErrorT)

dump m = withContext $ makeModule
    where
    makeModule ctx = runErrorT $ withModuleFromAST ctx m moduleString

main = do
    src <- getContents
    let Right ast = testParse src
    out <- dump $ cgModule ast
    either error putStrLn out
