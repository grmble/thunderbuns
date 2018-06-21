module Thunderbuns.GenPS where

import Control.Applicative
import Control.Lens
import Data.Proxy
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol)
import Language.PureScript.Bridge
import Servant.API
import Servant.Foreign
import Servant.PureScript
import Thunderbuns.Auth.Types
import Thunderbuns.Channel.Types
import Thunderbuns.Logging
import Thunderbuns.Server

myTypes :: [SumType 'Haskell]
myTypes =
  -- equal (Proxy :: Proxy Channel) $ mkSumType (Proxy :: Proxy Channel)
  [ mkSumType (Proxy :: Proxy Channel)
  , mkSumType (Proxy :: Proxy Msg)
  , mkSumType (Proxy :: Proxy NewMsg)
  , mkSumType (Proxy :: Proxy Priority)
  , mkSumType (Proxy :: Proxy UserPass)
  , mkSumType (Proxy :: Proxy Token)
  ]

generatePurescript :: IO ()
generatePurescript = do
  let frontEndRoot = "frontend/src"
  writeAPIModuleWithSettings mySettings frontEndRoot myBridgeProxy webAPI
  writePSTypes frontEndRoot (buildBridge myBridge) myTypes

-- | Move the types from Thunderbuns.Logging to Thunderbuns.WebAPI.Types
fixTypesModule :: BridgePart
fixTypesModule = do
  typeModule ^== "Thunderbuns.Logging" <|>
    typeModule ^== "Thunderbuns.Auth.Types" <|>
    typeModule ^== "Thunderbuns.Channel.Types" <|>
    typeModule ^== "Thunderbuns.Server.Auth"
  t <- view haskType
  TypeInfo (_typePackage t) "Thunderbuns.WebAPI.Types" (_typeName t) <$>
    psTypeParameters

-- | Substitue Map in generated API for HashMap String
--
-- XXX does not work - complains about "no generic for X"
fixHashmap :: BridgePart
fixHashmap = do
  typeName ^== "HashMap"
  typeModule ^== "Data.HashMap" <|> typeModule ^== "Data.HashMap.Base"
  TypeInfo "purescript-maps" "Data.Map" "Map" <$> psTypeParameters

-- | Set the PS api module to Thunderbuns.WebAPI
mySettings :: Settings
mySettings = set apiModuleName "Thunderbuns.WebAPI" (addReaderParam "Authorization" defaultSettings)

myBridge :: BridgePart
myBridge = defaultBridge <|> fixTypesModule <|> fixHashmap

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

-- XXX missing instance for the AuthProtect header
-- added as orphan, delete this when it appears in servant-foreign
instance ( KnownSymbol sym
         , HasForeignType lang ftype Text
         , HasForeign lang ftype sublayout
         ) =>
         HasForeign lang ftype (AuthProtect sym :> sublayout) where
  type Foreign ftype (AuthProtect sym :> sublayout) = Foreign ftype sublayout
  foreignFor lang Proxy Proxy req =
    foreignFor lang Proxy subP $ req & reqHeaders <>~ [HeaderArg arg]
    where
      arg =
        Arg
          { _argName = PathSegment "Authorization"
          , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text)
          }
      subP = Proxy :: Proxy sublayout
