module Handler.NewNews where

import Import
import Data.Maybe
import Yesod.Form.Bootstrap3
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Text.Blaze
import Data.List
import Control.Applicative ((<$>), (<*>))
import Data.Text           (Text)
import Data.Time           (Day)
import Yesod.Form.Jquery
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Types (PersistValue(PersistInt64))
import Data.Function (on)

data RateForm = RateForm
    {
       title    :: Text
    ,  rate     :: Text
    }
    deriving (Show, Eq, Read)

rateSampleForm :: Form RateForm
rateSampleForm = renderBootstrap3 BootstrapBasicForm $ RateForm
    <$> areq textField "Название заведения: " Nothing
    <*> areq textField "Отзыв: " Nothing


getNewNewsR :: Handler Html
getNewNewsR = do
    (formWidget, formEnctype) <- generateFormPost rateSampleForm
    let submission = Nothing :: Maybe RateForm
        handlerName = "getNewNewsR" :: Text

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Cafe Adviser - страница отзывов"
        $(widgetFile "news/new")

postNewNewsR :: Handler Html
postNewNewsR = do
    ((result, formWidget), formEnctype) <- runFormPost rateSampleForm
    let handlerName = "postNewNewsR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Cafe Adviser - старница отзывов"
        $(widgetFile "news/new")

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
