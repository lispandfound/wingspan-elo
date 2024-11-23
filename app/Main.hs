module Main where

import Model
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Network.HTTP.Types (status400)
import Data.List as L
import Data.Text hiding (index)
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Database.Persist.Sqlite (fromSqlKey, entityVal, runSqlite, toSqlKey)
import qualified Database.Persist.Sqlite as Sq
import Web.Scotty as S
import Network.Wai.Middleware.Static
import HTMX
import Control.Monad.Extra (mconcatMapM)
import qualified Data.Aeson.Text as JSON

wordHtml :: Text -> Html
wordHtml word = docTypeHtml $ do
  H.head $ do
    H.title "Word"
  H.body $ do
    h1 ("Hello world: " <> toHtml word)




scripts :: Html
scripts = do
  H.link ! A.rel "stylesheet" ! A.href "https://unpkg.com/almond.css@latest/dist/almond.lite.min.css"
  H.script ! A.src "https://unpkg.com/htmx.org@1.9.2" $ mempty

index :: PlayerId -> Player -> Maybe Avatar -> Ranking -> [(PlayerId, Player, Ranking)] -> Html
index topId topPlayer topAvatar topRank rankings = docTypeHtml $ do
    H.head $ do
        H.title "Wingspan ELO Rankings"
        H.meta ! A.charset "UTF-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        scripts
    H.body $ do
        H.header $ do
            H.div ! A.style "text-align: center; margin-bottom: 20px;" $ do
                H.h1 "Wingspan ELO Rankings"

        mainContent
    where
    mainContent = H.div ! A.style "margin: 0 auto; max-width: 1200px;" $ do
        H.div ! A.class_ "top-players" ! A.style "display: flex; justify-content: space-around; margin: 20px 0;" $ do
            playerCard 
        searchBar
        rankingsTable

    searchBar = H.div ! A.style "margin: 20px 0; text-align: center;" $ do
        H.form ! A.action "#" ! A.method "get" $ do
            H.input ! A.type_ "text" ! A.name "search" ! A.placeholder "Search player..."
            H.button ! A.type_ "submit" $ "Search"
    playerLink :: PlayerId -> Player -> Html
    playerLink playerId player = H.a ! (A.href . textValue $ "/players/" <> (pack . show . fromSqlKey $ playerId)) $ toHtml (playerName player)
    playerCard = H.div ! A.style "text-align: center;" $ do
        maybe mempty ((! A.style "width: 150px; height: 150px; border-radius: 50%;") . avatarImage) (topAvatar)
        H.h3 "Top Player" 
        playerLink topId topPlayer
        H.p $ do
            "Country: "
            H.strong $ H.toHtml (playerCountry  topPlayer)
        H.p $ do
            "ELO: "
            H.strong $ H.toHtml (rankingElo topRank)

    rankingsTable = H.table ! A.style "width: 100%; border-collapse: collapse; margin: 20px 0;" $ do
        H.thead $ do
            H.tr $ do
                H.th "Rank"
                H.th "Player"
                H.th "Country"
                H.th "ELO"
        H.tbody $ mapM_ tableRow exampleData

    tableRow (rank, (playerId, player, ranking)) = H.tr $ do
        H.td $ H.toHtml rank
        H.td $ H.toHtml (playerLink playerId player)
        H.td $ H.toHtml (playerCountry player)
        H.td $ H.toHtml (round (rankingElo ranking) :: Int)

    exampleData :: [(Int, (PlayerId, Player, Ranking))]
    exampleData = L.zip [1..] rankings


playerProfile :: Player -> Maybe Avatar -> [Ranking] -> Html
playerProfile player avatar rankings = docTypeHtml $ do
    H.head $ do
        H.title "Player Profile - Wingspan Rankings"
        H.meta ! A.charset "UTF-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        scripts
        H.script ! A.src "https://cdn.plot.ly/plotly-2.35.2.min.js" $ mempty
    H.body $ do
        H.header $ do
            H.div ! A.style "text-align: center; margin-bottom: 20px;" $ do
                H.h1 "Player Profile"
        mainContent
  where
    mainContent = H.div ! A.style "margin: 0 auto; max-width: 1200px;" $ do
        profileHeader
        rankingHistorySection rankings

    profileHeader = H.div ! A.style "text-align: center; margin: 20px;" $ do
        -- Avatar Image
        maybe mempty ((! A.style "width: 150px; height: 150px; border-radius: 50%;") . avatarImage) avatar
        -- Player Info: Name, Country, and ELO aligned in a single block
        H.h2 (H.toHtml $ playerName player) -- Player Name
        H.p ! A.style "display: inline;" $ do
            "Country: "
            H.strong (H.toHtml $ playerCountry player) -- Country
            br
            "Current ELO: "
            case rankings of
               (rank:_) -> H.strong (H.toHtml . (round :: Double -> Int) . rankingElo $ rank) -- ELO
               _ -> mempty
            
        
                  
rankingHistorySection ::        [Ranking] -> Html
rankingHistorySection rankings  = H.section $ do
    -- Add your ranking history content here
    h1 "Ranking History" ! A.style "text-align: center;"
    H.div ! A.id "ranking-graph" $ mempty
    H.script $ do
      "plot = document.getElementById('ranking-graph');"
      "Plotly.newPlot(plot, [{"
      "x:"
      H.toHtml $ JSON.encodeToLazyText (L.map rankingTimestamp rankings)
      ","
      "y:"
      H.toHtml $ JSON.encodeToLazyText (L.map rankingElo rankings)
      ","
      "type: 'scatter',"
      "}])"
    
avatarImage :: Avatar -> Html
avatarImage avatar = do
      H.img
            ! A.id "avatar-image"
            ! A.src ("/avatars/" <> (textValue $ avatarKey avatar) <> ".jpg")
            ! A.alt (textValue $ avatarName avatar)
            

registrationPage :: [Avatar] -> Html
registrationPage avatars = docTypeHtml $ do
    H.head $ do
        H.title "Player Registration - Wingspan Rankings"
        H.meta ! A.charset "UTF-8"
        H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
        scripts
    H.body $ do
        H.header $ do
            H.div ! A.style "text-align: center; margin-bottom: 20px;" $ do
                H.h1 "Player Registration"
        mainContent
  where
    avatarDisplay = H.div ! A.style "text-align: center; margin-bottom: 20px;" $ do
      avatarImage (Avatar "placehold" "Placeholder") ! A.style "width: 150px; height: 150px; border: 2px solid #ccc;"
    mainContent = H.div ! A.style "margin: 0 auto; max-width: 600px; padding: 20px; border: 1px solid #ddd; border-radius: 8px;" $ do
        H.form ! A.action "/submit-registration" ! A.method "post" ! A.style "display: flex; flex-direction: column; gap: 15px;" $ do
            avatarDisplay

            -- Name input
            H.div ! A.style "display: flex; justify-content: space-between; align-items: center;" $ do
                H.label ! A.for "name" ! A.style "margin-right: 20px;" $ "Name:"
                H.input ! A.type_ "text" ! A.id "name" ! A.name "name" ! A.required "true" ! A.placeholder "Enter your name"

            -- Country input
            H.div ! A.style "display: flex; justify-content: space-between; align-items: center;" $ do
                H.label ! A.for "country" ! A.style "margin-right: 20px;" $ "Country:"
                H.input ! A.type_ "text" ! A.id "country" ! A.name "country" ! A.required "true" ! A.placeholder "Enter your country"

            -- Avatar selection (Dropdown)
            H.div ! A.style "display: flex; justify-content: space-between; align-items: center;" $ do
                H.label ! A.for "avatar" ! A.style "margin-right: 20px;" $ "Mascot:"
                H.select ! A.id "avatar" ! A.name "avatar" $
                  mconcatMapM (\avatar -> H.option ! A.value (textValue $ avatarKey avatar) ! hxSwap "outerHTML" ! hxGet ("/inject/avatars/" <> (textValue $ avatarKey avatar)) ! hxTarget "#avatar-image" $ H.toHtml (avatarName avatar)) avatars
            -- Submit button
            H.div ! A.style "text-align: center; margin-top: 20px;" $ do
                H.button ! A.type_ "submit" ! A.style "width: 100%; padding: 10px;" $ "Register"


main :: IO ()
main = do
  runSqlite dbPath $ do
    initializeDB
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    get "/" (do
        rankings <- runSqlite dbPath $ getRankings
        let (topId, topPlayer, topRank) = L.head rankings
        topAvatar <- runSqlite dbPath $ playerAvatar topPlayer
        S.html $ renderHtml (index topId topPlayer topAvatar topRank rankings))
    get "/inject/avatars/:key" $ do
        key <- pathParam "key"
        avatar <- runSqlite dbPath $ avatarForKey key
        maybe next (S.html . renderHtml . avatarImage) avatar
    get "/players/:id" (do
        id <- toSqlKey <$> pathParam "id"
        playerRet <- runSqlite dbPath $ Sq.get id
        case playerRet of
            Just player -> do
              rankings <- runSqlite dbPath $ getPlayerRankings id
              avatar <- runSqlite dbPath $ playerAvatar player
              S.html $ renderHtml (playerProfile player avatar rankings)
            Nothing -> next)
    get "/register" $ do
      avatars <- L.map entityVal <$> (runSqlite dbPath $ Sq.selectList [] [])
      S.html (renderHtml $ registrationPage avatars)

    post "/submit-registration" $ do
      name <- S.formParam "name"
      country <- S.formParam "country"
      key <- S.formParam "avatar"

      playerKey <- runSqlite dbPath $ do
        avatarId <- avatarKeyToId key
        playerKey <- maybe (pure Nothing) (fmap pure . Sq.insert . Player name country) avatarId
        maybe (pure Nothing) (fmap (const playerKey) . Sq.insert . defaultRanking) playerKey
      maybe (status status400 >> S.text ("Invalid avatar key " <> TL.fromStrict key)) (redirect . ("/players/" <>) . TL.pack . show . fromSqlKey) playerKey


    where dbPath = "test.db"
