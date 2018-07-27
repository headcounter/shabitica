{-# LANGUAGE OverloadedStrings #-}
module Types
    ( TxnMail(..)
    , SimpleMail(..)
    , Address(..)
    , JsonResponse(..)
    , RenderedMail(..)
    ) where

import Data.Maybe (catMaybes)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Aeson ((.:), (.:?), (.=), object)
import Data.Aeson.Types (Parser)
import Network.Mail.Mime (Address(..))

import qualified Data.Text.Lazy as TL
import qualified Data.Aeson as J
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

-- XXX: Still needed for NixOS 18.03 which has base-4.10.1.0.
-- TODO: Bump cabal requirements to base-4.11 after NixOS 18.09 was released.
import Data.Semigroup (Semigroup((<>)))

data TxnMail = TxnMail
    { txnEmailType         :: Text
    , txnTo                :: [Address]
    , txnVariables         :: J.Value
    , txnPersonalVariables :: M.HashMap Text J.Value
    } deriving Show

instance J.FromJSON TxnMail where
    parseJSON = J.withObject "TxnData" $ \obj -> do
        txnType <- obj .: "type"
        unless (txnType == ("email" :: Text)) $
            fail "\"type\" needs to be \"email\""
        mobj <- obj .: "data"
        TxnMail <$> mobj .: "emailType"
                <*> (mapM parseAddress =<< (mobj .: "to"))
                <*> (parseVars =<< (mobj .: "variables"))
                <*> (parseNestedVars =<< (mobj .: "personalVariables"))
      where
        extractItem :: J.Value -> Parser (Maybe (Text, J.Value))
        extractItem = J.withObject "VarItem" $ \i ->
            i .: "name" >>= (<$> (i .:? "content")) . fmap . (,)

        parseVars :: J.Value -> Parser J.Value
        parseVars = J.withArray "variables" $ \vars ->
            object . catMaybes <$> mapM extractItem (V.toList vars)

        extractRcpt :: J.Value -> Parser (Text, J.Value)
        extractRcpt = J.withObject "Recipient" $ \r ->
            (,) <$> r .: "rcpt" <*> (parseVars =<< (r .: "vars"))

        parseNestedVars :: J.Value -> Parser (M.HashMap Text J.Value)
        parseNestedVars = J.withArray "recipients" $ \rcpts ->
            M.fromList <$> mapM extractRcpt (V.toList rcpts)

        parseAddress :: J.Value -> Parser Address
        parseAddress = J.withObject "Address" $ \a ->
            Address <$> a .:? "name" <*> a .: "email"

data SimpleMail = SimpleMail
    { smTo      :: Address
    , smSubject :: Text
    , smText    :: Text
    , smHtml    :: Maybe Text
    } deriving Show

instance J.FromJSON SimpleMail where
    parseJSON = J.withObject "SimpleMail" $ \obj ->
        SimpleMail <$> fmap (Address Nothing) (obj .: "to")
                   <*> obj .: "subject"
                   <*> obj .: "text"
                   <*> obj .:? "html"

data JsonResponse = JsonErr Text | JsonOk deriving Show

instance J.ToJSON JsonResponse where
    toJSON JsonOk =
        J.object ["status" .= ("ok" :: Text)]
    toJSON (JsonErr e) =
        J.object ["status" .= ("error" :: Text), "message" .= e]

data RenderedMail = RenderedMail
    { subject :: Text
    , body    :: TL.Text
    } deriving (Show, Eq)

instance Semigroup RenderedMail where
    a <> b = a { subject = subject a <> subject b, body = body a <> body b }
