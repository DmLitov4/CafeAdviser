{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Handler.Home where

import Import
import Data.Maybe
import Yesod.Form.Bootstrap3
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

import           Control.Applicative ((<$>), (<*>))
import           Data.Text           (Text)
import           Data.Time           (Day)
import           Yesod
import           Yesod.Form.Jquery

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)


-- Define our data that will be used for creating the form.
data FileForm = FileForm
    {
       kind :: Text
    ,  cuisine :: Maybe Text
    ,  bill :: Maybe Double
    ,  city :: Text
    ,  area :: Maybe Text
    ,  feature :: Maybe Text
    ,  parking :: Bool
    ,  dancing :: Bool
    ,  garden :: Bool
    }
    deriving (Show, Eq, Read)

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> areq (selectFieldList [("ресторан" :: Text, "ресторан"),("кафе", "кафе"), ("бар", "бар")]) "Тип заведения:  * " Nothing
    <*> aopt (selectFieldList [("русская" :: Text, "русская"),("французская", "французская"), ("итальянская", "итальянская"), ("США", "США"), ("японская", "японская"), ("латиноамериканская", "латиноамериканская"), ("грузинская", "грузинская"), ("индийская", "индийская")]) "Предпочитаемая кухня:  " Nothing
    <*> aopt doubleField "Средний чек: " Nothing
    <*> areq (selectFieldList [("Ростов-на-Дону" :: Text, "Ростов-на-Дону")]) "Город:  * " Nothing
    <*> aopt (selectFieldList [("Центр" :: Text, "Центр"), ("Западный", "Западный"), ("Северный", "Северный"), ("Александровка", "Александровка"), ("Сельмаш", "Сельмаш")]) "Район:  " Nothing
    <*> aopt (selectFieldList [("День Рождения" :: Text, "День Рождения"), ("свадьба", "свадьба"), ("корпоратив", "корпоратив")]) "Подходит для:  " Nothing
    <*> areq boolField "Наличие парковки:  * " Nothing
    <*> areq boolField "Наличие танцплощадки: * " Nothing
    <*> areq boolField "Наличие террасы / двора: * " Nothing


getHomeR :: Handler Html
getHomeR = do
    cafeList <- runDB $ selectList [RestaurantsName ==. "Буковски"] [] --fetch data
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Cafe Adviser"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    cafeList <- runDB $ selectList [RestaurantsName ==. "Буковски"] [] ----fetch data
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Cafe Adviser"
        $(widgetFile "homepage")

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")


showRestaurants :: Entity Restaurants -> Widget
showRestaurants (Entity restaurantid restaurant) = do
      {-
        Here we will make sub-queries
      -}
      --kind <- handlerToWidget $ runDB $ get404 $ restaurantsKindId restaurant
      --city <- handlerToWidget $ runDB $ get404 $ restaurantsCityId restaurant
      --area <- handlerToWidget $ runDB $ get404 $ restaurantsAreaId restaurant
      --feature <- handlerToWidget $ runDB $ get404 $ restaurantsFeatureId restaurant
      [whamlet|
            <div .restaurant-info>
                <br><em>#{restaurantsName restaurant}</em>   <br>
                  <!--  <em>#{kindsKindname kind}</em>           <br> -->
                    <em>#{restaurantsBill restaurant}</em>   <br>
                  <!--  <em>#{citiesCityname city}</em>          <br> -->
                  <!--  <em>#{areasAreaname area}</em>           <br> -->
                  <!--  <em>#{featuresFeaturename feature}</em>  <br> -->
                    <em>#{restaurantsParking restaurant}</em><br>
                    <em>#{restaurantsDancing restaurant}</em><br>
                    <em>#{restaurantsGarden restaurant}</em> <br>
      |]
