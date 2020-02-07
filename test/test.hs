{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

import           Control.Exception (throwIO)
import           Control.Exception.Lifted (bracket_)
import           Control.Monad (when)
import           Control.Monad.Base (liftBase)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.State.Class (MonadState(..), modify, gets)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.Trans.State (execStateT)

import           Data.Foldable (for_)
import           Data.Function (on)
import qualified Data.List as List
import           Data.Maybe (listToMaybe, fromJust)
import           Data.Pool (Pool, createPool, withResource)
import           Data.Text (Text)

import           Database.PostgreSQL.Simple (Connection)
import           Database.PostgreSQL.Simple (connectPostgreSQL)
import           Database.PostgreSQL.Simple (execute_)
import           Database.PostgreSQL.Simple (close)
import           Database.Postgres.Temp (with, toConnectionString)

import           Hedgehog hiding (Command)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Tutorial (createTables)

import           Tutorial (User, NewUser(..))
import           Tutorial (Post, NewPost(..))
import           Tutorial (createUser, readUser, packUser, deleteUser)
import           Tutorial (createPost, readPost, packPost)
import           Tutorial (userCreatedAt, UserId, userId, userEmail)
import           Tutorial (postCreatedAt, postUserId)

main =
  tests

tests :: IO Bool
tests =
  withPool $ \pool ->
  checkParallel $ Group "Tutorial" [
      ("prop_tables", prop_tables pool)
    , ("prop_commands", prop_commands pool)
    ]

prop_tables :: Pool Connection -> Property
prop_tables pool =
  property $
    withResource pool . abort $ \conn ->
      evalIO $ createTables conn

abort :: MonadBaseControl IO m => (Connection -> m a) -> Connection -> m a
abort f conn =
  bracket_
    (liftBase (execute_ conn "BEGIN"))
    (liftBase (execute_ conn "ROLLBACK"))
    (f conn)

-- Make sure initdb executable is in PATH
-- echo "export PATH=$PATH:/usr/lib/postgresql/VERSION/bin/" >> /home/ubuntu/.bashrc
-- https://hackage.haskell.org/package/tmp-postgres-1.34.1.0/docs/Database-Postgres-Temp.html
withPool :: (Pool Connection -> IO a) -> IO a
withPool io =
  (either throwIO pure =<<) .
  with $ \db -> do
    let connect = connectPostgreSQL (toConnectionString db)
    pool <- createPool connect close 2 60 10
    io pool

------------------------------------------------------------------------------

-- Make sure your Hedgehog import is `hiding (Command)`
-- You're building something much simpler that serves the same purpose.
data Command =
    CreateUser Text Text     -- name / email
  | DeleteUser Int           -- user-index
  | CreatePost Int Text Text -- user-index / title / body
    deriving (Eq, Ord, Show)

genCreateUser :: Gen Command
genCreateUser = do
  name <- Gen.element ["stephanie", "lennart", "simon"]
  pure $
    CreateUser name (name <> "@haskell.land")

genDeleteUser :: Gen Command
genDeleteUser =
  DeleteUser
    <$> Gen.int (Range.constant 0 50)

-- You can generate just about anything
-- here, you'll see why later.
genUserIx :: Gen Int
genUserIx =
  Gen.int (Range.constant 0 50)

genCreatePost :: Gen Command
genCreatePost =
  CreatePost
    <$> genUserIx
    <*> Gen.element ["C", "C++", "Haskell", "Rust", "JavaScript"]
    <*> Gen.element ["fast", "slow", "best", "worst"]

genCommand :: Gen Command
genCommand =
  Gen.choice [
      genCreateUser
    , genDeleteUser
    , genCreatePost
    ]

data Model =
  Model {
      modelUsers :: [User]
    , modelPosts :: [Post]
    } deriving (Eq, Ord, Show)

modelAddUser :: User -> Model -> Model
modelAddUser user x =
  x { modelUsers = modelUsers x <> [user] }

execCreateUser :: (
    MonadState Model m
  , MonadIO m
  , MonadTest m
  )
  => Connection
  -> Text
  -> Text
  -> m ()
execCreateUser conn name email = do
  let new = NewUser name email
  uid <- evalIO $ createUser conn new
  mgot <- evalIO $ readUser conn uid
  got <- eval $ fromJust mgot

  let want = packUser uid (userCreatedAt got) new
  want === got

  -- Track in the model that a user was created.
  -- Importantly, this means their UserId is known.
  modify (modelAddUser want)
  label "CreateUser"

modelRemoveUser :: UserId -> Model -> Model
modelRemoveUser uid x =
  x { modelUsers = List.filter ((/= uid) . userId) (modelUsers x) }

modelUserHasPosts :: UserId -> Model -> Bool
modelUserHasPosts uid x =
  any ((uid ==) . postUserId) (modelPosts x)

execDeleteUser :: (
    MonadState Model m
  , MonadIO m
  , MonadTest m
  )
  => Connection
  -> Int
  -> m ()
execDeleteUser conn userIx = do
  muser <- gets (lookupIx userIx . modelUsers)
  case muser of
    Nothing ->
      -- failed precondition
      pure ()
    Just user -> do
   -- You need to add this check.
   -- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
      active <- gets (modelUserHasPosts (userId user))
      if active then
        -- failed precondition
        -- possible improvement: make sure deleteUser throws
        pure ()
      else do
        evalIO $ deleteUser conn (userId user)
        modify (modelRemoveUser (userId user))
        label "DeleteUser"

modelAddPost :: Post -> Model -> Model
modelAddPost post x =
  x { modelPosts = modelPosts x <> [post] }

-- Lookup an element at the specified index
-- or a modulo thereof if past the end.
lookupIx :: Int -> [a] -> Maybe a
lookupIx ix = \case
  [] ->
    Nothing
  xs ->
    listToMaybe (drop (ix `mod` length xs) (reverse xs))

execCreatePost :: (
    MonadState Model m
  , MonadIO m
  , MonadTest m
  )
  => Connection
  -> Int
  -> Text
  -> Text
  -> m ()
execCreatePost conn userIx title body = do
  muser <- gets (lookupIx userIx . modelUsers)
  case muser of
    Nothing ->
      -- failed precondition, skip
      pure ()
    Just user -> do
      let new = NewPost (userId user) title body
      pid <- evalIO $ createPost conn new
      mgot <- evalIO $ readPost conn pid
      got <- eval $ fromJust mgot

      let want = packPost pid (postCreatedAt got) new
      want === got

      modify (modelAddPost want)
      label "CreatePost"

execCommands :: (
    MonadIO m
  , MonadTest m
  )
  => Connection
  -> [Command]
  -> m Model
execCommands conn xs =
  flip execStateT (Model [] []) . for_ xs $ \case
    CreateUser name email ->
      execCreateUser conn name email
    DeleteUser userIx ->
      execDeleteUser conn userIx
    CreatePost userIx title body ->
      execCreatePost conn userIx title body

assertNoDuplicateEmails :: MonadTest m => Model -> m ()
assertNoDuplicateEmails model = do
  -- check for email duplicates
  let want = List.nubBy ((==) `on` userEmail) (modelUsers model)
  let got = modelUsers model
  want === got

prop_commands :: Pool Connection -> Property
prop_commands pool =
  property $ do
    commands <- forAll $ Gen.list (Range.constant 0 100) genCommand
    withResource pool . abort $ \conn -> do
      evalIO $ createTables conn
      model <- execCommands conn commands

      assertNoDuplicateEmails model

      -- add some labels for how many posts were generated
      let n = length (modelPosts model)
      when (n >= 10) $ label "Posts 10+"
      when (n >= 20) $ label "Posts 20+"
      when (n >= 30) $ label "Posts 30+"
