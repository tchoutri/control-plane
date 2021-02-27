module CLI where

import           Colourista.IO              (greenMessage, redMessage)
import qualified Data.Password.Argon2       as Argon2
import           Data.Time
import           Data.UUID.V4
import           Options.Applicative

import           DB.Helpers  (runDB)
import           DB.User     (NewUser (..), User (..), UserId (..))
import qualified DB.User     as DB
import           Environment (ControlPlaneEnv (..), mkEnv)
import qualified Server      as Server

newtype Options = Options Command

data Command = AddUser Text Text Text
             | DeleteUser Text
             | StartServer

cliMain :: IO ()
cliMain = runOptions =<< execParser (parseOptions `withInfo` "CLI tool for control-plane")

parseOptions :: Parser Options
parseOptions = Options <$> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ 
     command "add-user" (parseAddUser `withInfo` "Add a user to the database")
  <> command "delete-user" (parseDeleteUser `withInfo` "Delete a user from the database")
  <> command "start" (parseStart `withInfo` "Start control-plane")

parseAddUser :: Parser Command
parseAddUser = 
  AddUser <$> argument str (metavar "USERNAME")
          <*> argument str (metavar "DISPLAYNAME")
          <*> argument str (metavar "PASSWORD")

parseDeleteUser :: Parser Command
parseDeleteUser =
  DeleteUser <$> argument str (metavar "Username")

parseStart :: Parser Command
parseStart = pure StartServer

runOptions :: Options -> IO ()
runOptions (Options (AddUser username displayName password)) =
  addUser (NewUser username displayName (Argon2.mkPassword password))
runOptions (Options (DeleteUser username)) = deleteUser username
runOptions (Options StartServer) = Server.startService

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

addUser :: NewUser -> IO ()
addUser NewUser{..} = do
  env <- mkEnv
  let pool = pgPool env
  hashedPassword <- DB.hashPassword password
  timestamp <- getCurrentTime
  userId <- UserId <$> nextRandom
  let user = User{userId=userId, username=username, displayName=displayName
                               , password=hashedPassword, createdAt=timestamp
                               , updatedAt=timestamp}
  result <- runDB pool $ DB.insertUser user
  case result of
    Left _ -> redMessage "[!] Could not insert user."
    Right _  -> do
      u <- runDB pool $ DB.getUserByUsername username
      greenMessage "[+] User created"
      putTextLn $ show u

deleteUser :: Text -> IO ()
deleteUser username = do
  env <- mkEnv
  let pool = pgPool env
  result <- runDB pool $ DB.getUserByUsername username
  case result of
    Right user -> void $ runDB pool $ DB.deleteUser (userId user)
    Left  err  -> redMessage $ "[!] Could not delete user. " <> show err
