module Control.Monad.RW

import public Control.Monad.Identity
import public Control.Monad.Reader
import public Control.Monad.Writer

%default total

public export
RW : Type -> Type -> Type -> Type
RW r w a = WriterT w (ReaderT r Identity) a

export
MkRW : (r -> (a, w)) -> RW r w a
MkRW f = WR (RD $ Id . f)

export
runRW : RW r w a -> r -> (a, w)
runRW rw = runIdentity . runReaderT (runWriterT rw)
