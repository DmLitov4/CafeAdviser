module Handler.RestAdd where

import Yesod.Form.Bootstrap3
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.Maybe

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Types (PersistValue(PersistInt64))

import Import


data AddRestForm = AddRestForm
    {
       name    :: Text  --name of restaurant
    ,  kind    :: Text  --type is cafe, bar or restaurant
    ,  cuisine :: Text
    ,  bill    :: Int
    ,  city    :: Text
    ,  area    :: Text
    ,  feature :: Text
    ,  parking :: Bool
    ,  dancing :: Bool
    ,  garden  :: Bool
    ,  description :: Text
    ,  image :: Text
    }
    deriving (Show, Eq, Read)

getForeignKind :: Text -> Key Kinds
getForeignKind t
               | t == "ресторан" = toSqlKey 1
               | t == "кафе" = toSqlKey 2
               | t == "бар" = toSqlKey 3
               | otherwise = toSqlKey (-1)

getForeignCuisine :: Text -> Key Cuisines
getForeignCuisine t
                  | t == "русская" = toSqlKey  1
                  | t == "французская" = toSqlKey 2
                  | t == "итальянская" = toSqlKey 3
                  | t == "США" = toSqlKey 4
                  | t == "японская" = toSqlKey 5
                  | t == "латиноамериканская" = toSqlKey 6
                  | t == "грузинская" = toSqlKey 7
                  | t == "индийская" = toSqlKey 8
                  | otherwise = toSqlKey (-1)

getForeignArea :: Text -> Key Areas
getForeignArea t
               | t == "Центр" = toSqlKey 1
               | t == "Западный" = toSqlKey 2
               | t == "Северный" = toSqlKey 3
               | t == "Александровка" = toSqlKey 4
               | t == "Сельмаш" = toSqlKey 5
               | otherwise = toSqlKey (-1)

getForeignFeature :: Text -> Key Features
getForeignFeature t
                  | t == "День Рождения" = toSqlKey(1)
                  | t == "свадьба" = toSqlKey(2)
                  | t == "корпоратив" = toSqlKey(3)
                  | otherwise = toSqlKey (-1)

getForeignCity :: Key Cities
getForeignCity = toSqlKey 1

addForm :: Form AddRestForm
addForm = renderBootstrap3 BootstrapBasicForm $ AddRestForm
    <$> areq textField        "Название заведения  " Nothing
    <*> areq (selectFieldList [("ресторан" :: Text, "ресторан"),("кафе", "кафе"), ("бар", "бар")]) "Тип заведения:  " Nothing
    <*> areq (selectFieldList [("русская" :: Text, "русская"),("французская", "французская"), ("итальянская", "итальянская"), ("США", "США"), ("японская", "японская"), ("латиноамериканская", "латиноамериканская"), ("грузинская", "грузинская"), ("индийская", "индийская")]) "Предпочитаемая кухня:  " Nothing
    <*> areq intField      "Средний чек: " Nothing
    <*> areq (selectFieldList [("Ростов-на-Дону" :: Text, "Ростов-на-Дону")]) "Город:   " Nothing
    <*> areq (selectFieldList [("Центр" :: Text, "Центр"), ("Западный", "Западный"), ("Северный", "Северный"), ("Александровка", "Александровка"), ("Сельмаш", "Сельмаш")]) "Район:  " Nothing
    <*> areq (selectFieldList [("День Рождения" :: Text, "День Рождения"), ("свадьба", "свадьба"), ("корпоратив", "корпоратив")]) "Подходит для:  " Nothing
    <*> areq boolField        "Наличие парковки:    " Nothing
    <*> areq boolField        "Наличие танцплощадки:  " Nothing
    <*> areq boolField        "Наличие террасы / двора:   " Nothing
    <*> areq textField        "Описание" Nothing
    <*> areq textField        "Ссылка на изображение" Nothing


getRestAddR :: Handler Html
getRestAddR = do
    (formWidget, formEnctype) <- generateFormPost addForm
    defaultLayout $ do
      setTitle "Cafe Adviser - добавить заведение"
      $(widgetFile "addrest")

postRestAddR :: Handler Html
postRestAddR = do
    ((result, formWidget), formEnctype) <- runFormPost addForm
    case result of
      FormSuccess newRest -> do
                          --Now we inserts new restaurant in Restaurants table--
                          restId <- runDB $ insert $ Restaurants (name newRest) (getForeignKind (kind newRest)) (getForeignCuisine (cuisine newRest)) (bill newRest) (getForeignCity) (getForeignArea (area newRest)) (getForeignFeature (feature newRest)) (boolToInt (parking newRest) ) (boolToInt (dancing newRest) ) (boolToInt (garden newRest) ) (description newRest) (image newRest)
                          maybeRest <- runDB $ get restId
                          case maybeRest of
                              Nothing -> error "Our restaurant was not added"
                              _ -> do
                                defaultLayout $ do
                                  setTitle "Cafe Adviser - добавить заведение"
                                  $(widgetFile "addrest")

                          defaultLayout $ do
                            setTitle "Cafe Adviser - добавить заведение"
                            $(widgetFile "addrest")

      _ -> defaultLayout $ do
             setTitle "Cafe Adviser - добавить заведение"
             $(widgetFile "addrest")

boolToInt :: Bool -> Int
boolToInt t = if t then 1 else 0
