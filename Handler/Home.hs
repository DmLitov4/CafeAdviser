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
    ,  parking :: Maybe Bool
    ,  dancing :: Maybe Bool
    ,  garden  :: Maybe Bool
    }
    deriving (Show, Eq, Read)

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> areq (selectFieldList [("ресторан" :: Text, "ресторан"),("кафе", "кафе"), ("бар", "бар")]) "Тип заведения:  * " Nothing
    <*> aopt (selectFieldList [("русская" :: Text, "русская"),("французская", "французская"), ("итальянская", "итальянская"), ("США", "США"), ("японская", "японская"), ("латиноамериканская", "латиноамериканская"), ("грузинская", "грузинская"), ("индийская", "индийская")]) "Предпочитаемая кухня:  " Nothing
    <*> aopt doubleField      "Средний чек: " Nothing
    <*> areq (selectFieldList [("Ростов-на-Дону" :: Text, "Ростов-на-Дону")]) "Город:  * " Nothing
    <*> aopt (selectFieldList [("Центр" :: Text, "Центр"), ("Западный", "Западный"), ("Северный", "Северный"), ("Александровка", "Александровка"), ("Сельмаш", "Сельмаш")]) "Район:  " Nothing
    <*> aopt (selectFieldList [("день рождения" :: Text, "день рождения"), ("свадьба", "свадьба"), ("корпоратив", "корпоратив")]) "Подходит для:  " Nothing
    <*> aopt boolField        "Наличие парковки:  * " Nothing
    <*> aopt boolField        "Наличие караоке / танцплощадки: * " Nothing
    <*> aopt boolField        "Наличие террасы / двора: * " Nothing


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
               | t == "день рождения" = 1
               | t == "свадьба" = 2
               | t == "корпоратив" = 3
               | otherwise = -1

getFromBool :: Bool -> Int
getFromBool b
          | b == True = 1
          | b == False = 0
          | otherwise = -1

countMatchesBill :: FileForm -> Entity Restaurants -> Integer
countMatchesBill cf (Entity restaurantid restaurant)
                                          | (abs(fromMaybe 0 (bill cf) - fromIntegral(restaurantsBill restaurant)) < 200.0) = 10
                                          | otherwise = 0

countMatchesKind :: FileForm -> Entity Restaurants -> Integer
countMatchesKind cf (Entity restaurantid restaurant) = if (fromSqlKey(restaurantsKindId restaurant) == getForeignKind (kind cf)) then 18 else 0

countMatchesCuisine :: FileForm -> Entity Restaurants -> Integer
countMatchesCuisine cf (Entity restaurantid restaurant) = if (fromSqlKey(restaurantsCuisineId restaurant) == getForeignCuisine (fromMaybe " " (cuisine cf))) then 13 else 0

countMatchesArea :: FileForm -> Entity Restaurants -> Integer
countMatchesArea cf (Entity restaurantid restaurant) = if (fromSqlKey(restaurantsAreaId restaurant) == getForeignArea (fromMaybe " " (area cf))) then 5 else 0

countMatchesFeature :: FileForm -> Entity Restaurants -> Integer
countMatchesFeature cf (Entity restaurantid restaurant) = if (fromSqlKey(restaurantsFeatureId restaurant) == getForeignArea (fromMaybe " " (feature cf))) then 3 else 0

countMatchesParking :: FileForm -> Entity Restaurants -> Integer
countMatchesParking cf (Entity restaurantid restaurant) = if (fromIntegral(restaurantsParking restaurant) == getFromBool (fromMaybe False (parking cf)) && (parking cf) /= Nothing) then 3 else 0

countMatchesDancing :: FileForm -> Entity Restaurants -> Integer
countMatchesDancing cf (Entity restaurantid restaurant) = if (fromIntegral(restaurantsDancing restaurant) == getFromBool (fromMaybe False (dancing cf)) && (dancing cf) /= Nothing) then 3 else 0

countMatchesGarden :: FileForm -> Entity Restaurants -> Integer
countMatchesGarden cf (Entity restaurantid restaurant) = if (fromIntegral(restaurantsGarden restaurant) == getFromBool (fromMaybe False (garden cf)) && (garden cf) /= Nothing) then 3 else 0

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
                                       parkingmatch = countMatchesParking cf curcafe
                                       dancingmatch = countMatchesDancing cf curcafe
                                       gardenmatch = countMatchesGarden cf curcafe

                                   acc Data.List.++ [(kindmatch + billmatch + cuisinematch + areamatch + featurematch, [curcafe])]
      -- and here we create results list [Entity Restaurants]
            createresult acc2 x = acc2 Data.List.++ (snd x)

--filterRestaurants2 :: FileForm -> [Entity Restaurants] -> [(Int, Entity Restaurants)]
filterRestaurants2 cf restlist = (foldl createresult [] (Data.List.take 3 (Data.List.reverse(Data.List.sortBy (compare `on` fst)(foldl allpairs [] restlist)))))
      -- here we create a list that contains all cafes in pairs like (number of matches, Entity Restaurants)
      where allpairs acc curcafe = do
                                   let billmatch = countMatchesBill cf curcafe
                                       kindmatch = countMatchesKind cf curcafe
                                       cuisinematch = countMatchesCuisine cf curcafe
                                       areamatch = countMatchesArea cf curcafe
                                       featurematch = countMatchesFeature cf curcafe

                                   acc Data.List.++ [(kindmatch + billmatch + cuisinematch + areamatch + featurematch, curcafe)]
      -- and here we create results list [Entity Restaurants]
            createresult acc2 x = acc2 Data.List.++ [(fst x, snd x)]


showRestaurants :: Entity Restaurants -> Widget
showRestaurants (Entity restaurantid restaurant) = do
      kind    <- handlerToWidget $ runDB $ get404 (restaurantsKindId restaurant)
      cuisine <- handlerToWidget $ runDB $ get404 (restaurantsCuisineId restaurant)
      city    <- handlerToWidget $ runDB $ get404 (restaurantsCityId restaurant)
      area    <- handlerToWidget $ runDB $ get404 (restaurantsAreaId restaurant)
      feature <- handlerToWidget $ runDB $ get404 (restaurantsFeatureId restaurant)

      [whamlet|
            <div .restaurant-info>
                      <p style="padding: 10px">
                         <img class="cafeimg" src="#{restaurantsImage restaurant}" width="120" class="lefti" />

                         &nbsp <br> &nbsp &nbsp  <b>#{restaurantsName restaurant}</b>  <br>
                         <br>
                         <br>
                         <br> <i>#{restaurantsDescription restaurant}</i> <br>
                         Тип заведения: <em>  #{kindsKindname (kind)}</em>           <br>
                         Основная кухня: <em> #{cuisinesCuisinename (cuisine)} </em> <br>
                         Средний чек (на одного человека): <em> #{restaurantsBill restaurant}  </em>   <br>
                         Город: <em> #{citiesCityname (city)} </em>          <br>
                         Район: <em> #{areasAreaname area}</em>             <br>
                         Подходит для: <em> #{featuresFeaturename feature}</em>    <br>
                         Наличие парковки: <em> #{restaurantsParking restaurant}</em>  <br>
                         Наличие караоке / танцевальной площадки: <em> #{restaurantsDancing restaurant}</em>  <br>
                         Наличие террасы / двора: <em> #{restaurantsGarden restaurant}</em>   <br>
      |]
