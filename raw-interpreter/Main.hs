--import Control.Lens
import Control.Monad.IO.Class
import Control.Exception.Safe
import Control.Monad.Except
--import Control.Monad (when)
import System.IO
--import System.FilePath
--import System.Directory (createDirectoryIfMissing)
import Data.IORef
--import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Pact.Core.Builtin
import Pact.Core.Environment
import Pact.Core.Persistence.MockPersistence
import Pact.Core.Pretty
import Pact.Core.Repl.Compile
import Pact.Core.Repl.Utils
import Pact.Core.Serialise
import Pact.Core.Info
--import Pact.Core.Errors
--import Pact.Core.Coverage (showReport)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Started"
  pdb <- mockPactDb serialisePact_repl_fileLocSpanInfo
  ee <- defaultEvalEnv pdb replBuiltinMap

  ref <- newIORef (mkReplState' ee displayOutput)
  evalReplM ref loop  >>= \case
    Left err -> do
      putStrLn "Exited repl session with error:"
      putStrLn $ T.unpack $ replError (SourceCode "(interactive)" "") err
    _ -> pure ()
  where
  displayOutput :: (Pretty a, MonadIO m) => FileLocSpanInfo -> a -> m ()
  displayOutput _ = liftIO . putStrLn . show . pretty

  catch' ma = catchAny ma (\e -> liftIO ( putStrLn (show e)) *> loop)
  defaultSrc = SourceCode "(interactive)" mempty

  handleLine src = catch' $ do
    replCurrSource .== defaultSrc{_scPayload=src}
    eout <- tryError (interpretReplProgramDirect (SourceCode "(interactive)" src))
    case eout of
      Right _ -> pure ()
      Left err -> do
        rstate <- getReplState
        let renderedError = renderLocatedPactErrorFromState rstate err
        replCurrSource .== defaultSrc
        liftIO $ putStrLn (T.unpack renderedError)


  loop ::  ReplM ReplCoreBuiltin ()
  loop = do
    eof <- liftIO isEOF

    if eof
      then liftIO $ putStrLn "goodbye"

      else do
         src <- liftIO T.getLine
         if T.null src
            then loop
            else handleLine src >> loop