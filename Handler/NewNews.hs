module Handler.NewNews where

import Import

getNewNewsR :: Handler Html
getNewNewsR = do
	            (_, user) <- requireAuthPair
	            defaultLayout $ do
	             
	              $(widgetFile "news/new")