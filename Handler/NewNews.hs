module Handler.NewNews where

import Import
import Data.Maybe
import Yesod.Form.Bootstrap3
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Text.Blaze
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
    <$> areq textField (bfs ("Название заведения: " :: Text)) Nothing
    <*> areq textField (bfs ("Отзыв: " :: Text)) Nothing

getNewNewsR :: Handler Html
getNewNewsR = do
    newsList <- runDB $ selectList [] []
    (formWidget, formEnctype) <- generateFormPost rateSampleForm

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Cafe Adviser - страница отзывов"
        $(widgetFile "news/new")

postNewNewsR :: Handler Html
postNewNewsR = do
    newsList <- runDB $ selectList [] []
    ((result, formWidget), formEnctype) <- runFormPost rateSampleForm
    case result of
            FormSuccess news -> do
            	                _ <- runDB $ insert $ News (title news) $ (rate news)
            	                defaultLayout $ do
            	                let (commentFormId, commentTextareaId, commentListId) = commentIds
                                aDomId <- newIdent
                                setTitle "Cafe Adviser - страница отзывов"
            	                $(widgetFile "news/new")
            _ -> defaultLayout $ do
                              let (commentFormId, commentTextareaId, commentListId) = commentIds
                              aDomId <- newIdent
                              setTitle "Cafe Adviser - страница отзывов"
                              $(widgetFile "news/new")

allNews :: Entity News -> Widget
allNews (Entity rateid rate) = do
  [whamlet|
    <div>
       <ul>
          <li>
            <p>
              <em>Название заведения:</em> <b> #{newsTitle rate} </b> <br>
              <em>#{newsInfotext rate} </em> <br> |]

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
