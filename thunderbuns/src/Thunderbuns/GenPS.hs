module Thunderbuns.GenPS where

import Control.Applicative
import Control.Lens
import Data.Proxy
import Data.Semigroup ((<>))
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import Language.PureScript.Bridge
import System.IO
import Thunderbuns.Auth.Types
import Thunderbuns.Channel.Types
import Thunderbuns.Logging

myTypes :: [SumType 'Haskell]
myTypes
  -- equal (Proxy :: Proxy Channel) $ mkSumType (Proxy :: Proxy Channel)
 =
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
  writePSTypes frontEndRoot (buildBridge myBridge) myTypes
  postprocessTypes

postprocessTypes :: IO ()
postprocessTypes = do
  let fn = "frontend/src/Thunderbuns/WebAPI/Types.purs"
  withFile fn ReadWriteMode $ \h -> do
    ls <- lines' h []
    hSeek h AbsoluteSeek 0
    LT.hPutStr h $ postprocess ls
    -- hGetContents / lines won't work, hGetContents semi-closes the handle
  where
    lines' :: Handle -> [LT.Text] -> IO [LT.Text]
    lines' h !acc = do
      b <- hIsEOF h
      if b
        then pure $ reverse acc
        else do
          next <- LT.hGetLine h
          lines' h (next : acc)
    postprocess xlines =
      LT.intercalate
        "\n"
        (prefix xlines <> imports xlines <> imports' <> suffix xlines <> suffix' <>
         [])
      where
        notImport = not . LT.isPrefixOf "import"
        isImportOrEmpty s = s == "" || LT.isPrefixOf s "import"
        prefix = takeWhile notImport
        imports = dropWhile notImport . takeWhile isImportOrEmpty
        suffix = dropWhile notImport . dropWhile isImportOrEmpty
        decodeInstance klass =
          "instance decode" <> klass <> " :: Decode " <> klass <>
          " where decode = genericDecode (defaultOptions { unwrapSingleConstructors = true })"
        encodeInstance klass =
          "instance encode" <> klass <> " :: Encode " <> klass <>
          " where encode = genericEncode (defaultOptions { unwrapSingleConstructors = true })"
        eqInstance klass = "derive instance eq" <> klass <> " :: Eq " <> klass
        ordInstance klass =
          "derive instance ord" <> klass <> " :: Ord " <> klass
        imports' =
          [ "import Foreign.Generic (defaultOptions, genericEncode, genericDecode)"
          , "import Foreign.Class (class Decode, class Encode)"
          ]
        suffix' =
          concatMap
            (\klass ->
               [ decodeInstance klass
               , encodeInstance klass
               , eqInstance klass
               , ordInstance klass
               ])
            ["Channel", "Msg", "NewMsg", "Priority", "UserPass", "Token"]

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

myBridge :: BridgePart
myBridge = defaultBridge <|> fixTypesModule <|> fixHashmap

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy
