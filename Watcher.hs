{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

import Control.Applicative

import Control.Arrow
import System.Directory
import System.Locale
import Data.Time
import System.Random
import Utils
import Common
import Network.Mail.SMTP
import Network.Mail.Mime hiding (simpleMail)
import Network.Socket.Internal
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as B

toMailLine :: TodoItem -> T.Text
toMailLine (TodoItem {..}) =T.pack $ message ++ " from " ++ timeStr
    where timeStr = formatTime' "%D %T" dateAdded

makeTodoMail :: [TodoItem] -> Mail
makeTodoMail ts = simpleMail from to cc bcc subject [body]
    where from    = Address Nothing "mailboxpointer@gmail.com"
          to      = [Address (Just "Izaak Meckler") "izaakmeckler@me.com"]
          cc      = []
          bcc     = []
          subject = "Do these things!"
          body    = plainTextPart . T.unlines $ map toMailLine ts

main = B.putStr =<< renderMail' . makeTodoMail . reverse =<< getTodoList "main"