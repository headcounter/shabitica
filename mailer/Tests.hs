{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Data.Aeson ((.=), object)
import NeatInterpolation
import Test.Hspec

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

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

welcome :: TxnMail
welcome = TxnMail
    { txnEmailType = "welcome"
    , txnTo = [Address (Just "foo") "foo@example.org"]
    , txnVariables = object
        ["BASE_URL" .= T.pack "https://habitica.example.org"]
    , txnPersonalVariables = Just $ object
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
inviteFriend = TxnMail
    { txnEmailType = "invite-friend"
    , txnTo = [Address (Just "foo") "foo@example.org"]
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

main :: IO ()
main = hspec $ do
    describe "reset-password" $ do
        let rendered = renderSimpleMail resetPassword

        it "has correct subject" $
            subject rendered `shouldBe` "Password Reset for Habitica"
        it "has correct body" $
            body rendered `shouldBe` resetPasswordBody

    describe "welcome" $ do
        let rendered = snd $ renderTxnMail welcome

        it "has correct subject" $
            subject rendered `shouldBe` "Welcome to Habitica"
        it "has correct body" $
            body rendered `shouldBe` welcomeBody

    describe "invite-friend" $ do
        let rendered = snd $ renderTxnMail inviteFriend

        it "has correct subject" $
            subject rendered `shouldBe` "Invitation to Habitica from foo"
        it "has correct body" $
            body rendered `shouldBe` inviteFriendBody
