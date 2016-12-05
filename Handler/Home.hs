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
import Control.Monad.IO.Class (liftIO)
import Control.Applicative


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

getForeignKind :: Text -> Int64
getForeignKind t
                | t == "ресторан" = 1
                | t == "кафе" = 2
                | t == "бар" = 3
                | otherwise = -1

getForeignCuisine :: Text -> Int64
getForeignCuisine t
                | t == "русская" = 1
                | t == "французская" = 2
                | t == "итальянская" = 3
                | t == "США" = 4
                | t == "японская" = 5
                | t == "латиноамериканская" = 6
                | t == "грузинская" = 7
                | t == "индийская" = 8
                | otherwise = -1

getForeignArea :: Text -> Int64
getForeignArea t
               | t == "Центр" = 1
               | t == "Западный" = 2
               | t == "Северный" = 3
               | t == "Александровка" = 4
               | t == "Сельмаш" = 5
               | otherwise = -1

getForeignFeature :: Text -> Int64
getForeignFeature t
               | t == "День Рождения" = 1
               | t == "свадьба" = 2
               | t == "корпоратив" = 3
               | otherwise = -1

countMatchesBill :: FileForm -> Entity Restaurants -> Integer
countMatchesBill cf (Entity restaurantid restaurant)
                                          | (abs(fromMaybe 0 (bill cf) - fromIntegral(restaurantsBill restaurant)) < 200.0) = 9                                         
                                          | otherwise = 0

countMatchesKind :: FileForm -> Entity Restaurants -> Integer
countMatchesKind cf (Entity restaurantid restaurant) = if (fromSqlKey(restaurantsKindId restaurant) == getForeignKind (kind cf)) then 10 else 0

countMatchesCuisine :: FileForm -> Entity Restaurants -> Integer
countMatchesCuisine cf (Entity restaurantid restaurant) = if (fromSqlKey(restaurantsCuisineId restaurant) == getForeignCuisine (fromMaybe " " (cuisine cf))) then 8 else 0

countMatchesArea :: FileForm -> Entity Restaurants -> Integer
countMatchesArea cf (Entity restaurantid restaurant) = if (fromSqlKey(restaurantsAreaId restaurant) == getForeignArea (fromMaybe " " (area cf))) then 4 else 0

countMatchesFeature :: FileForm -> Entity Restaurants -> Integer
countMatchesFeature cf (Entity restaurantid restaurant) = if (fromSqlKey(restaurantsFeatureId restaurant) == getForeignArea (fromMaybe " " (feature cf))) then 3 else 0

-- in one fold we take 5 best pairs (Mathces, Cafe) from list sorted by matches and then we just take snd from them and add to result list
filterRestaurants :: FileForm -> [Entity Restaurants] -> [Entity Restaurants]
filterRestaurants cf restlist = (foldl createresult [] (Data.List.take 3 (Data.List.reverse(Data.List.sortBy (compare `on` fst)(foldl allpairs [] restlist)))))
      -- here we create a list that contains all cafes in pairs like (number of matches, Entity Restaurants)
      where allpairs acc curcafe = do                                   
                                   let billmatch = countMatchesBill cf curcafe
                                       kindmatch = countMatchesKind cf curcafe
                                       cuisinematch = countMatchesCuisine cf curcafe
                                       areamatch = countMatchesArea cf curcafe
                                       featurematch = countMatchesFeature cf curcafe
                                   acc Data.List.++ [(kindmatch + billmatch + cuisinematch + areamatch + featurematch, [curcafe])]
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
