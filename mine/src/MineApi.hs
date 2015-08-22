module MineApi where

import Servant.API
import Data.Aeson
import qualified Data.Map as Map
import Data.Time.Clock
import Control.Error.Util

type MineAPI
  = "resource" :> (ReqBody "id" Id :> Post '[JSON] Resource
              :<|> "check" :> Capture "resource" Resource :> Get '[JSON] IsValid)

newtype Resource = Resource { unResource :: String }
  deriving (Eq, Show, FromJSON, ToJSON)

newtype Id = Id { unId :: String }
  deriving (Eq, Show, FromJSON, ToJSON)

data IsValid = Valid | Invalid
  deriving (Eq, Show, FromJSON, ToJSON)

-- | Make a new resource. Returns
--   - 429 if you've made too many requests and aren't entitled to more resources
--   - 400 if your Id is not valid
--   - 200 with resource otherwise
mkResource :: MVar (Map.Map Id UTCTime) -> Id -> EitherT ServantErr IO Resource
mkResource idsvar (Id x) = do
    ids <- liftIO $ takeMVar idsvar
    case Map.lookup x ids of
      Nothing -> throwError err400
      Just t  -> do
        now <- liftIO getCurrentTime
        when (t > now) $
          writeMVar idsvar (addUTCTime penaltyTime t) >> throwError err429
        return newResource

penaltyTime :: DiffTime
penaltyTime = _

exhaustionTime :: DiffTime
exhaustionTime = _
