module Handler.RestAdd where

import Yesod.Form.Bootstrap3
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Import


data AddRestForm = AddRestForm
    {
       name    :: Text  --name of restaurant
    ,  kind    :: Text  --type is cafe, bar or restaurant
    ,  cuisine :: Text
    ,  bill    :: Double
    ,  city    :: Text
    ,  area    :: Text
    ,  feature :: Text
    ,  parking :: Bool
    ,  dancing :: Bool
    ,  garden  :: Bool
    }
    deriving (Show, Eq, Read)

addForm :: Form AddRestForm
addForm = renderBootstrap3 BootstrapBasicForm $ AddRestForm
    <$> areq textField        "Название заведения  * " Nothing
    <*> areq (selectFieldList [("ресторан" :: Text, "ресторан"),("кафе", "кафе"), ("бар", "бар")]) "Тип заведения:  * " Nothing
    <*> areq (selectFieldList [("русская" :: Text, "русская"),("французская", "французская"), ("итальянская", "итальянская"), ("США", "США"), ("японская", "японская"), ("латиноамериканская", "латиноамериканская"), ("грузинская", "грузинская"), ("индийская", "индийская")]) "Предпочитаемая кухня:  " Nothing
    <*> areq doubleField      "Средний чек: " Nothing
    <*> areq (selectFieldList [("Ростов-на-Дону" :: Text, "Ростов-на-Дону")]) "Город:  * " Nothing
    <*> areq (selectFieldList [("Центр" :: Text, "Центр"), ("Западный", "Западный"), ("Северный", "Северный"), ("Александровка", "Александровка"), ("Сельмаш", "Сельмаш")]) "Район:  " Nothing
    <*> areq (selectFieldList [("День Рождения" :: Text, "День Рождения"), ("свадьба", "свадьба"), ("корпоратив", "корпоратив")]) "Подходит для:  " Nothing
    <*> areq boolField        "Наличие парковки:  *  " Nothing
    <*> areq boolField        "Наличие танцплощадки: *  " Nothing
    <*> areq boolField        "Наличие террасы / двора: *  " Nothing


getRestAddR :: Handler Html
getRestAddR = do
    (formWidget, formEnctype) <- generateFormPost addForm
    defaultLayout $ do
      $(widgetFile "addrest")

postRestAddR :: Handler Html
postRestAddR = do
    ((result, formWidget), formEnctype) <- runFormPost addForm
    case result of
      FormSuccess newRest -> addToDB newRest
      _ -> defaultLayout $(widgetFile "addrest")


--Here we add new restaurants to Data Base
addToDB :: AddRestForm -> Handler Html
addToDB rf = undefined
