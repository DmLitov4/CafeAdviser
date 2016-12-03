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

data FileForm = FileForm
    {
       kind    :: Text
    ,  cuisine :: Maybe Text
    ,  bill    :: Maybe Double
    ,  city    :: Text
    ,  area    :: Maybe Text
    ,  feature :: Maybe Text
    ,  parking :: Bool
    ,  dancing :: Bool
    ,  garden  :: Bool
    }
    deriving (Show, Eq, Read)

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> areq (selectFieldList [("ресторан" :: Text, "ресторан"),("кафе", "кафе"), ("бар", "бар")]) "Тип заведения:  * " Nothing
    <*> aopt (selectFieldList [("русская" :: Text, "русская"),("французская", "французская"), ("итальянская", "итальянская"), ("США", "США"), ("японская", "японская"), ("латиноамериканская", "латиноамериканская"), ("грузинская", "грузинская"), ("индийская", "индийская")]) "Предпочитаемая кухня:  " Nothing
    <*> aopt doubleField      "Средний чек: " Nothing
    <*> areq (selectFieldList [("Ростов-на-Дону" :: Text, "Ростов-на-Дону")]) "Город:  * " Nothing
    <*> aopt (selectFieldList [("Центр" :: Text, "Центр"), ("Западный", "Западный"), ("Северный", "Северный"), ("Александровка", "Александровка"), ("Сельмаш", "Сельмаш")]) "Район:  " Nothing
    <*> aopt (selectFieldList [("День Рождения" :: Text, "День Рождения"), ("свадьба", "свадьба"), ("корпоратив", "корпоратив")]) "Подходит для:  " Nothing
    <*> areq boolField        "Наличие парковки:  * " Nothing
    <*> areq boolField        "Наличие танцплощадки: * " Nothing
    <*> areq boolField        "Наличие террасы / двора: * " Nothing


getHomeR :: Handler Html
getHomeR = do
    cafeList <- runDB $ selectList [] [] 
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
    cafeList <- runDB $ selectList [] [] 
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

countMatchesBill :: FileForm -> Entity Restaurants -> Integer
countMatchesBill cf (Entity restaurantid restaurant)
                                          | (abs(fromJust(bill cf) - fromIntegral(restaurantsBill restaurant)) < 400.0) = 1                                         
                                          | otherwise = 0

-- in one fold we take 5 best pairs (Mathces, Cafe) from list sorted by matches and then we just take snd from them and add to result list
filterRestaurants :: FileForm -> [Entity Restaurants] -> [Entity Restaurants]
filterRestaurants cf restlist = (foldl createresult [] (Data.List.take 1 (Data.List.reverse(Data.List.sortBy (compare `on` fst)(foldl allpairs [] restlist)))))
      -- here we create a list that contains all cafes in pairs like (number of matches, Entity Restaurants)
      where allpairs acc curcafe = do
                                   --k <- runDB $ get404 (restaurantsKindId curcafe)
                                   let billmatch = countMatchesBill cf curcafe
                                   acc Data.List.++ [(billmatch, [curcafe])]
      -- and here we create results list [Entity Restaurants]
            createresult acc2 x = acc2 Data.List.++ (snd x)

showRestaurants :: Entity Restaurants -> Widget
showRestaurants (Entity restaurantid restaurant) = do
      kind    <- handlerToWidget $ runDB $ get404 (restaurantsKindId restaurant)
      cuisine <- handlerToWidget $ runDB $ get404 (restaurantsCuisineId restaurant)
      city    <- handlerToWidget $ runDB $ get404 (restaurantsCityId restaurant)
      area    <- handlerToWidget $ runDB $ get404 (restaurantsAreaId restaurant)
      feature <- handlerToWidget $ runDB $ get404 (restaurantsFeatureId restaurant)
    
      [whamlet|
            <div .restaurant-info>
              <br><em><b>#{restaurantsName restaurant}</b></em>     <br>
                    <em>Тип заведения: #{kindsKindname (kind)}</em>           <br>
                    <em>Основная кухня: #{cuisinesCuisinename (cuisine)} </em> <br>
                    <em>Средний чек (на одного человека): #{restaurantsBill restaurant}</em>     <br>
                    <em>Город: #{citiesCityname (city)}</em>          <br> 
                    <em>Район: #{areasAreaname area}</em>             <br> 
                    <em>Подходит для: #{featuresFeaturename feature}</em>    <br> 
                    <em>Наличие парковки: #{restaurantsParking restaurant}</em>  <br>
                    <em>Наличие караоке / танцевальной площадки: #{restaurantsDancing restaurant}</em>  <br>
                    <em>Наличие террасы / двора: #{restaurantsGarden restaurant}</em>   <br>
      |]
