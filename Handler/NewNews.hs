module Handler.NewNews where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

getNewNewsR :: Handler Html
getNewNewsR = do
	            (_, user) <- requireAuthPair
	            defaultLayout $ do
	              let (commentFormId, commentTextareaId, commentListId) = commentIds
	              aDomId <- newIdent
	              setTitle . toHtml $ userIdent user <> "' - Страница новостей"	             
	              $(widgetFile "news/new")

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")