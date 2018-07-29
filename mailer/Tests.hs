{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Aeson ((.=), object)
import NeatInterpolation
import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.HashMap.Strict as M

import Types
import Render

-- XXX: Still needed for NixOS 18.03 which has base-4.10.1.0.
-- TODO: Bump cabal requirements to base-4.11 after NixOS 18.09 was released.
import Data.Semigroup (Semigroup((<>)))

space :: T.Text
space = " "

resetPassword :: SimpleMail
resetPassword = SimpleMail
    { smTo = "foo@example.org"
    , smSubject = "Password Reset for Habitica"
    , smText = "If you requested a password reset for foo on Habitica, head"
            <> " to http://example.org/.../reset-password-set-new-one?code="
            <> "123456 to set a new one. The link will expire after 24 hours."
            <> " If you haven't requested a password reset, please ignore"
            <> " this email."
    , smHtml = Just $ "If you requested a password reset for <strong>foo</str"
                   <> "ong> on Habitica, <a href=\"http://example.org/.../"
                   <> "reset-password-set-new-one?code=123456\">click"
                   <> " here</a> to set a new one. The link will expire"
                   <> " after 24 hours.<br/><br>If you haven't requested a"
                   <> " password reset, please ignore this email."
    }

resetPasswordBody :: TL.Text
resetPasswordBody = TL.fromStrict [text|
    If you requested a password reset for foo on Habitica, head to
    http://example.org/.../reset-password-set-new-one?code=123456 to set a
    new one. The link will expire after 24 hours. If you haven't requested a
    password reset, please ignore this email.
|]

commonTxnAddr :: T.Text
commonTxnAddr = "foo@example.org"

commonTxnRecip :: Address
commonTxnRecip = Address (Just "foo") commonTxnAddr

commonTxnMail :: TxnMail
commonTxnMail = TxnMail
    { txnEmailType = "unknown"
    , txnTo = [commonTxnRecip]
    , txnVariables = object []
    , txnPersonalVariables = M.empty
    }

welcome :: TxnMail
welcome = commonTxnMail
    { txnEmailType = "welcome"
    , txnVariables = object
        ["BASE_URL" .= T.pack "https://habitica.example.org"]
    , txnPersonalVariables = M.singleton commonTxnAddr $ object
        ["RECIPIENT_UNSUB_URL" .= T.pack "/email/unsubscribe?code=1234"]
    }

welcomeBody :: TL.Text
welcomeBody = TL.fromStrict [text|
    Hello stranger,

    Welcome to the self-hosted Habitica instance at
    https://habitica.example.org/.

    To get started simply head over to https://habitica.example.org/ and log
    in.
    --$space
    Self-hosted Habitica instance at https://habitica.example.org/
    Unsubscribe: https://habitica.example.org/email/unsubscribe?code=1234
|]

inviteFriend :: TxnMail
inviteFriend = commonTxnMail
    { txnEmailType = "invite-friend"
    , txnVariables = object
        [ "LINK"     .= T.pack "/static/front?groupInvite=abcdef0123456789"
        , "INVITER"  .= T.pack "foo"
        , "BASE_URL" .= T.pack "https://habitica.example.org"
        ]
    , txnPersonalVariables = M.singleton commonTxnAddr $ object
        [ "RECIPIENT_NAME" .= T.pack "bar"
        , "RECIPIENT_UNSUB_URL" .= T.pack "/email/unsubscribe?code=1234"
        ]
    }

inviteFriendBody :: TL.Text
inviteFriendBody = TL.fromStrict [text|
    Hello bar,

    foo has invited you to the self-hosted Habitica instance at
    https://habitica.example.org/.

    Please head to the following URL to accept your invitation:

    https://habitica.example.org/static/front?groupInvite=abcdef0123456789
    --$space
    Self-hosted Habitica instance at https://habitica.example.org/
    Unsubscribe: https://habitica.example.org/email/unsubscribe?code=1234
|]

inviteCollectionQuest :: TxnMail
inviteCollectionQuest = commonTxnMail
    { txnEmailType = "invite-collection-quest"
    , txnVariables = object
        [ "QUEST_NAME" .= T.pack
              "Attack of the Mundane, Part 1: Dish Disaster!"
        , "INVITER"    .= T.pack "foo"
        , "BASE_URL"   .= T.pack "https://habitica.example.org"
        , "PARTY_URL"  .= T.pack "/party"
        ]
    , txnPersonalVariables = M.singleton commonTxnAddr $ object
        [ "RECIPIENT_NAME" .= T.pack "bar"
        , "RECIPIENT_UNSUB_URL" .= T.pack "/email/unsubscribe?code=1234"
        ]
    }

inviteCollectionQuestBody :: TL.Text
inviteCollectionQuestBody = TL.fromStrict [text|
    Hello bar,

    You were invited to the Collection Quest Attack of the Mundane, Part 1:
    Dish Disaster!

    To join, please head over to:

    https://habitica.example.org/party
    --$space
    Self-hosted Habitica instance at https://habitica.example.org/
    Unsubscribe: https://habitica.example.org/email/unsubscribe?code=1234
|]

inviteBossQuest :: TxnMail
inviteBossQuest = commonTxnMail
    { txnEmailType = "invite-boss-quest"
    , txnVariables = object
        [ "QUEST_NAME" .= T.pack "The Basi-List"
        , "INVITER"    .= T.pack "foo"
        , "BASE_URL"   .= T.pack "https://habitica.example.org"
        , "PARTY_URL"  .= T.pack "/party"
        ]
    , txnPersonalVariables = M.singleton commonTxnAddr $ object
        [ "RECIPIENT_NAME" .= T.pack "bar"
        , "RECIPIENT_UNSUB_URL" .= T.pack "/email/unsubscribe?code=1234"
        ]
    }

inviteBossQuestBody :: TL.Text
inviteBossQuestBody = TL.fromStrict [text|
    Hello bar,

    You were invited to the Boss Quest The Basi-List

    To join, please head over to:

    https://habitica.example.org/party
    --$space
    Self-hosted Habitica instance at https://habitica.example.org/
    Unsubscribe: https://habitica.example.org/email/unsubscribe?code=1234
|]

questStarted :: TxnMail
questStarted = commonTxnMail
    { txnEmailType = "quest-started"
    , txnVariables = object
        [ "BASE_URL"   .= T.pack "https://habitica.example.org"
        , "PARTY_URL"  .= T.pack "/party"
        ]
    , txnPersonalVariables = M.singleton commonTxnAddr $ object
        [ "RECIPIENT_NAME" .= T.pack "bar"
        , "RECIPIENT_UNSUB_URL" .= T.pack "/email/unsubscribe?code=1234"
        ]
    }

questStartedBody :: TL.Text
questStartedBody = TL.fromStrict [text|
    Hello bar,

    The quest you have joined has just started. Please head over to your
    party to see the details:

    https://habitica.example.org/party
    --$space
    Self-hosted Habitica instance at https://habitica.example.org/
    Unsubscribe: https://habitica.example.org/email/unsubscribe?code=1234
|]

newPm :: TxnMail
newPm = commonTxnMail
    { txnEmailType = "new-pm"
    , txnVariables = object
        [ "BASE_URL" .= T.pack "https://habitica.example.org"
        , "SENDER"   .= T.pack "foo"
        ]
    , txnPersonalVariables = M.singleton commonTxnAddr $ object
        [ "RECIPIENT_NAME" .= T.pack "bar"
        , "RECIPIENT_UNSUB_URL" .= T.pack "/email/unsubscribe?code=1234"
        ]
    }

newPmBody :: TL.Text
newPmBody = TL.fromStrict [text|
    Hello bar,

    You got a new private message from foo.
    --$space
    Self-hosted Habitica instance at https://habitica.example.org/
    Unsubscribe: https://habitica.example.org/email/unsubscribe?code=1234
|]

main :: IO ()
main = hspec $ do
    describe "reset-password" $ do
        let rendered = renderSimpleMail resetPassword

        it "has correct subject" $
            subject rendered `shouldBe` "Password Reset for Habitica"
        it "has correct body" $
            body rendered `shouldBe` resetPasswordBody

    describe "welcome" $ do
        let rendered = snd $ renderTxnMail commonTxnRecip welcome

        it "has correct subject" $
            subject rendered `shouldBe` "Welcome to Habitica"
        it "has correct body" $
            body rendered `shouldBe` welcomeBody

    describe "invite-friend" $ do
        let rendered = snd $ renderTxnMail commonTxnRecip inviteFriend

        it "has correct subject" $
            subject rendered `shouldBe` "Invitation to Habitica from foo"
        it "has correct body" $
            body rendered `shouldBe` inviteFriendBody

    describe "invite-collection-quest" $ do
        let rendered = snd $ renderTxnMail commonTxnRecip inviteCollectionQuest

        it "has correct subject" $
            subject rendered `shouldBe`
                "New Collection Quest: Attack of the Mundane,"
             <> " Part 1: Dish Disaster!"
        it "has correct body" $
            body rendered `shouldBe` inviteCollectionQuestBody

    describe "invite-boss-quest" $ do
        let rendered = snd $ renderTxnMail commonTxnRecip inviteBossQuest

        it "has correct subject" $
            subject rendered `shouldBe` "New Boss Quest: The Basi-List"
        it "has correct body" $
            body rendered `shouldBe` inviteBossQuestBody

    describe "quest-started" $ do
        let rendered = snd $ renderTxnMail commonTxnRecip questStarted

        it "has correct subject" $
            subject rendered `shouldBe` "Habitica Quest started"
        it "has correct body" $
            body rendered `shouldBe` questStartedBody

    describe "new-pm" $ do
        let rendered = snd $ renderTxnMail commonTxnRecip newPm

        it "has correct subject" $
            subject rendered `shouldBe` "New private message from foo"
        it "has correct body" $
            body rendered `shouldBe` newPmBody
