{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Render (renderTxnMail, renderSimpleMail) where

import Data.Aeson (object, (.=))
import Control.Arrow (first, second, (>>>), (***))
import Text.Mustache (Template(..), PName(PName), Node(..),
                      MustacheWarning(..), displayMustacheWarning,
                      renderMustacheW)
import Text.Mustache.Compile.TH (compileMustacheDir, compileMustacheText)
import Text.Wrap (wrapText, defaultWrapSettings)

import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Types (SimpleMail(..), TxnMail(..), RenderedMail(..))

-- XXX: Still needed for NixOS 18.03 which has base-4.10.1.0.
-- TODO: Bump cabal requirements to base-4.11 after NixOS 18.09 was released.
import Data.Semigroup (Semigroup((<>)))

fixNode :: Node -> Node
fixNode (EscapedVar key) = UnescapedVar key
fixNode node             = node

fixTemplate :: Template -> Template
fixTemplate tpl = tpl { templateCache = fmap fixNode <$> templateCache tpl }

templates :: Template
templates = fixTemplate $(compileMustacheDir "unknown" "templates/")

greeting :: Template
greeting = $(compileMustacheText "greeting" $ "Hello "
    <> "{{#p.RECIPIENT_NAME}}{{& p.RECIPIENT_NAME }}{{/p.RECIPIENT_NAME}}"
    <> "{{^p.RECIPIENT_NAME}}stranger{{/p.RECIPIENT_NAME}}"
    <> ",\n\n")

footer :: Template
footer = $(compileMustacheText "footer" . T.intercalate "\n" $
    [ "\n-- "
    , "Self-hosted Habitica instance at {{& v.BASE_URL }}/"
    , "Unsubscribe: {{& v.BASE_URL }}{{& p.RECIPIENT_UNSUB_URL }}\n"
    ])

{- Not yet supported:

   admin-feedback
   donation
   flag-report-to-mods-with-comments
   gifted-gems
   gifted-subscription
   group-cancel-subscription
   group-member-join
   group-subscription-begins
   guild-invite-rescinded
   invite-boss-quest
   invite-collection-quest
   invited-guild
   invited-party
   kicked-from-guild
   kicked-from-party
   new-pm
   party-invite-rescinded
   quest-started
   subscription-begins
   unflag-report-to-mods
   won-challenge
-}

getTemplateName :: T.Text -> T.Text
getTemplateName "invite-friend-guild" = "invite-friend"
getTemplateName name                  = name

getTemplate :: T.Text -> Template
getTemplate name = Template
    { templateCache = cached
    , templateActual = if exists then pname else "unknown"
    }
  where
    pname = PName $ getTemplateName name
    exists = M.member pname cached
    cached = templateCache templates

processBody :: TL.Text -> TL.Text
processBody =
    mkParas . splitBody . TL.strip
  where
    splitBody = fmap (TL.strip . TL.replace "\n" " ") . TL.splitOn "\n\n"
    mkParas = TL.intercalate "\n\n" . fmap wrapPara
    wrapPara = TL.fromStrict . wrapText defaultWrapSettings 72 . TL.toStrict

postProcessRendered :: TL.Text -> RenderedMail
postProcessRendered rendered = uncurry RenderedMail $
    TL.toStrict *** processBody $ TL.breakOn "\n" rendered

postProcessWarnings :: Bool -> [MustacheWarning] -> [T.Text]
postProcessWarnings isUnknown warnings =
    fmap (T.pack . displayMustacheWarning) filtered
  where
    filtered = if isUnknown then filter wfilter warnings else warnings
    wfilter (MustacheDirectlyRenderedValue _) = True
    wfilter _                                 = False

renderTxnMail :: TxnMail -> ([T.Text], RenderedMail)
renderTxnMail txn = render $ object
    [ "emailType" .= txnEmailType txn
    , "v" .= txnVariables txn
    , "p" .= txnPersonalVariables txn
    ]
  where
    postProcess = postProcessWarnings isUnknown *** postProcessRendered
    isUnknown = templateActual tpl == "unknown"
    processPart = first (postProcessWarnings isUnknown)
              >>> second (RenderedMail "")
    render obj = processPart (renderMustacheW greeting obj)
              <> postProcess (renderMustacheW tpl obj)
              <> processPart (renderMustacheW footer obj)
    tpl = getTemplate (txnEmailType txn)

renderSimpleMail :: SimpleMail -> RenderedMail
renderSimpleMail sm = RenderedMail
    { subject = smSubject sm
    , body = flip TL.snoc '\n' . processBody . TL.fromStrict $ smText sm
    }
