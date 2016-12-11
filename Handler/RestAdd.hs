module Handler.RestAdd where

import Yesod.Form.Bootstrap3
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

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
                          --Insert data in Kinds, Cuisines, Cities, Areas and Features tables--
                          kindId <- runDB $ insert $ Kinds (kind newRest)                    --kind id in Kinds Table
                          cuisineId <- runDB $ insert $ Cuisines (cuisine newRest)            --cuisin id in Cuisines Table
                          cityId <- runDB $ insert $ Cities (city newRest)                   --city id in Cities Table
                          areaId <- runDB $ insert $ Areas (area newRest)                    --area id in Areas Table
                          featureId <- runDB $ insert $ Features (feature newRest)           --feature id in Features Table

                          --Now we inserts new restaurant in Restaurants table--
                          restId <- runDB $ insert $ Restaurants (name newRest) (kindId) (cuisineId) (bill newRest) (cityId) (areaId) (featureId) (boolToInt (parking newRest) ) (boolToInt (dancing newRest) ) (boolToInt (garden newRest) ) (description newRest) (image newRest)
                          maybeRest <- runDB $ get restId
                          case maybeRest of
                              Nothing -> error "insert error"
                              _ -> error "Ok"

                          defaultLayout $ do
                            setTitle "Cafe Adviser - добавить заведение"
                            $(widgetFile "addrest")

      _ -> defaultLayout $ do
             setTitle "Cafe Adviser - добавить заведение"
             $(widgetFile "addrest")

boolToInt :: Bool -> Int
boolToInt t = if t then 1 else 0
