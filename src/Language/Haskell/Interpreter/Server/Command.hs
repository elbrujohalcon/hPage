
module Language.Haskell.Interpreter.Server.Command (
    Command, CommandBuilder, action, caller,
    eval, kindOf, typeOf, loadModules, getLoadedModules,
    setTopLevelModules, getModuleExports,
    runCmd, Result, unwrapText, unwrapModules, unwrapElems
    )
where

import Control.Concurrent.Process hiding (init)
import qualified Language.Haskell.Interpreter as Hint
import Control.Monad.Error

type Result = Either Hint.InterpreterError SimpleResult 

data SimpleResult = Text String
                  | Ok
--             | Ok | True | False
                  | Modules [Hint.ModuleName]
                  | ModuleElems [Hint.ModuleElem]

instance Show SimpleResult where
    show (Text s) = s
    show (Modules ms) = show ms
    show (ModuleElems mes) = show mes
    show Ok = "Ok"

unwrapText :: SimpleResult -> String
unwrapText = show

unwrapModules :: SimpleResult -> [Hint.ModuleName]
unwrapModules (Modules ms) = ms
unwrapModules _ = [] 

unwrapElems :: SimpleResult -> [Hint.ModuleElem]
unwrapElems (ModuleElems mes) = mes
unwrapElems _ = []

data Action = Eval String
            | TypeOf String
            | KindOf String
            | LoadModules [Hint.ModuleName]
            | SetTopLevelModules [Hint.ModuleName]
            | GetModuleExports Hint.ModuleName
            | GetLoadedModules
    deriving (Show)

data Command = Cmd { action :: Action,
                     caller :: Handle Result }

instance Show Command where
    show = show . action

type CommandBuilder t = (Handle Result) -> t -> Command

eval, kindOf, typeOf :: CommandBuilder String
getModuleExports :: CommandBuilder Hint.ModuleName
loadModules, setTopLevelModules :: CommandBuilder [Hint.ModuleName]
getLoadedModules :: CommandBuilder ()

eval h s = Cmd {action = Eval s, caller = h}
kindOf h s = Cmd {action = KindOf s, caller = h}
typeOf h s = Cmd {action = TypeOf s, caller = h}
loadModules h ms = Cmd {action = LoadModules ms, caller = h} 
setTopLevelModules h ms = Cmd {action = SetTopLevelModules ms, caller = h}
getLoadedModules h _ = Cmd {action = GetLoadedModules, caller = h}
getModuleExports h m = Cmd {action = GetModuleExports m, caller = h}

runCmd :: Hint.MonadInterpreter m => Command -> ReceiverT r m Result
runCmd cmd = do
                res <- case action cmd of
                            Eval expr ->
                                execText (Hint.eval expr)
                            TypeOf expr ->
                                execText (Hint.typeOf expr)
                            KindOf expr ->
                                execText (Hint.kindOf expr)
                            LoadModules ms ->
                                exec (Hint.loadModules ms)
                            SetTopLevelModules ms ->
                                exec (Hint.setTopLevelModules ms)
                            GetModuleExports m ->
                                execElems (Hint.getModuleExports m)
                            GetLoadedModules ->
                                execModules Hint.getLoadedModules
                sendTo (caller cmd) $ res
                return res
    where execText    interp = (interp >>= return . Right . Text) `catchError` (return . Left)
          execModules interp = (interp >>= return . Right . Modules) `catchError` (return . Left)
          execElems   interp = (interp >>= return . Right . ModuleElems) `catchError` (return . Left)
          exec        interp = (interp >> return (Right Ok)) `catchError` (return . Left) 