module Main where

import qualified Data.IGraph as Graph
import Database.PostgreSQL.Simple
import Control.Exception (evaluate)
import System.Environment (getArgs)
import Data.String
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Control.Monad

getConnection :: IO Connection
getConnection = connect (defaultConnectInfo{connectUser = "giorgidz", connectDatabase = "giorgidz"})

getEdges :: Double -> IO [(Integer,Integer)]
getEdges day = do
  conn <- getConnection
  let query1 = "SELECT link_from_page, link_to_page FROM links WHERE link_added < " ++ show day ++ " AND link_removed > " ++  show day ++ " "
  result <- query_ conn (fromString query1)
  close conn
  return result

getPageIds :: IO [Integer]
getPageIds = do
  conn   <- getConnection
  result <- query_ conn (fromString ("SELECT DISTINCT scp_page FROM super_category_pages"))
  close conn
  return (map fromOnly result)

getRedirects :: Double -> IO (S.HashSet Integer)
getRedirects day = do
  conn   <- getConnection
  let query1 = unlines  [ "SELECT   rev_page AS page, max(rev_id) AS rev"
                        , "FROM     revision_stats, revisions"
                        , "WHERE    rev_id = rev_stat_revision"
                        , "AND      rev_page = rev_stat_page"
                        , "AND      rev_time < " ++ show day
                        , "GROUP BY rev_page"]
  let query2 = unlines  [ "SELECT   page"
                        , "FROM     revision_stats, (" ++ query1 ++ ") AS pr"
                        , "WHERE    rev_stat_revision = rev"
                        , "AND      rev_stat_page = page"
                        , "AND      rev_stat_is_redirect != 0"]
  result <- query_ conn (fromString query2)
  close conn
  return $ S.fromList $ map fromOnly result

resolveRedirects :: [(Integer,Integer)] -> S.HashSet Integer -> [(Integer,Integer)]
resolveRedirects eds reds = concatMap (func S.empty) eds
  where
  edsMap = M.fromList eds
  func vis (fr,to) = case (S.member fr reds,S.member to reds) of
                       (False,False) -> [(fr,to)]
                       (False,True)  -> case M.lookup to edsMap of
                                          Nothing  -> []
                                          Just to' | S.member to' vis -> []
                                                   | otherwise -> func (S.insert to vis) (fr,to')
                       (True,_)      -> []

insertResults :: [(Double,Integer,Integer,Double,Double)] -> IO ()
insertResults = mapM_ go
  where
  fields = "(network_stat_date,network_stat_page,network_stat_nodes,network_stat_closeness_in,network_stat_eigenvector)"
  go r@( network_stat_date
       , network_stat_page
       , network_stat_nodes
       , network_stat_closeness_in
       , network_stat_eigenvector
       ) = do
    conn <- getConnection
    res1 <- query conn
                  (fromString (unlines [ "SELECT network_stat_nodes        , "
                                       , "       network_stat_closeness_in , "
                                       , "       network_stat_eigenvector    "
                                       , "FROM   network_stats"
                                       , "WHERE  network_stat_date = ? AND network_stat_page = ?"
                                       ]))
                  (network_stat_date,network_stat_page)

    case res1 of
      []                  -> do _ <- execute conn (fromString ("INSERT INTO network_stats " ++ fields ++" VALUES (?,?,?,?,?)")) r
                                close conn
      (nsd,nsci,nsei) : _ -> do when (nsd  == (-999 :: Int))    $ fmap (const ()) $ execute conn (fromString ("UPDATE network_stats SET network_stat_nodes = ?        WHERE network_stat_date = ? AND network_stat_page = ?")) (network_stat_nodes,network_stat_date,network_stat_page)
                                when (nsci == (-999 :: Double)) $ fmap (const ()) $ execute conn (fromString ("UPDATE network_stats SET network_stat_closeness_in = ? WHERE network_stat_date = ? AND network_stat_page = ?")) (network_stat_closeness_in,network_stat_date,network_stat_page)
                                when (nsei == (-999 :: Double)) $ fmap (const ()) $ execute conn (fromString ("UPDATE network_stats SET network_stat_eigenvector = ?  WHERE network_stat_date = ? AND network_stat_page = ?")) (network_stat_eigenvector,network_stat_date,network_stat_page)
                                close conn

main :: IO ()
main = do
  args <- getArgs
  let day = case args of
              []      -> error "usage: dsh-wikipedia-centralities-day epoch_time_stamp"
              (s : _) -> read s

  rEdges     <- getEdges day
  rPageIds   <- getPageIds
  rRedirects <- getRedirects day

  rGraph                    <- evaluate $ Graph.make (resolveRedirects rEdges rRedirects)
  rNodeNumber               <- evaluate $ fromIntegral (Graph.nodeNumber rGraph)
  rClosenessIn              <- evaluate $ M.fromList [ (pid, Graph.closenessIn rGraph pid) | pid <- rPageIds ]
  rEigenVectorCentralities  <- evaluate $ M.fromList (Graph.eigenvectorCentrality rGraph)

  let na     = -99 :: Integer
  let nad    = -99 :: Double
  let result =  [ if Graph.member rGraph pid
                     then ( day
                          , pid
                          , rNodeNumber
                          , rClosenessIn M.! pid
                          , rEigenVectorCentralities M.! pid
                          )
                     else (day, pid, na, nad, nad)
                | pid <- rPageIds
                ]
  insertResults result