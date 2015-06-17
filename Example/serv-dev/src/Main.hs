{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TVar    (TVar, newTVar, readTVar,
                                                 writeTVar)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Logger           (runStderrLoggingT)
import           Control.Monad.Trans.Control    (MonadBaseControl (..))
import           Data.Monoid
import           Data.Text
import           Data.Time                      (UTCTime, getCurrentTime)
import           Database.Persist               (insert)
import           Database.Persist.Sqlite        (ConnectionPool, SqlPersistT,
                                                 runMigration, runSqlPool,
                                                 withSqlitePool)
import           Database.Persist.TH            (mkMigrate, mkPersist,
                                                 persistLowerCase, share,
                                                 sqlSettings)
import           Network.Wai.Application.Static (defaultWebAppSettings,
                                                 staticApp)
import           Network.Wai.Handler.Warp       (defaultSettings, runSettings,
                                                 setPort)
-- import           Network.Wai.Handler.WarpTLS    (runTLS, TLSSettings(..), tlsSettings)
import qualified Network.Wai.Handler.WebSockets as WS
import           Network.Wai.Middleware.Cors    (simpleCors)
import qualified Network.WebSockets             as WS
import qualified Network.WebSockets.Connection  as WS
import           Thrift.WSServer

import           Serv
import qualified Serv_Iface                     as SI
import qualified Serv_Types                     as ST

type People = [Text]
data Global = Global { pool :: ConnectionPool, ppl :: TVar People }

-- Example data type that can be stored in the db
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
MyData
    name Text
    numb Int
    time UTCTime Maybe
|]


nobody :: [Text]
nobody = []

class DBer a where
    doDB :: MonadBaseControl IO m => a -> SqlPersistT m b -> m b

instance DBer Global where
    doDB app action = runSqlPool action (pool app)

instance SI.Serv_Iface Global where
    ping self = do
        now <- getCurrentTime

        -- Example database insert
        let db = doDB self
        db $ insert $ MyData "my" 2 (Just now)

        print "PING!"

    myOp self sargs msg = do
        print $ "ServArgs: " <> show sargs
        print $ "Message: " <> msg
        let retVal = 32
        print $ "Sending back " <> show retVal
        return retVal

main = runStderrLoggingT $ withSqlitePool "resources/db/example.db" 5 $ \pool -> do
    -- Automatically build my database
    runSqlPool (runMigration migrateAll) pool

    st <- liftIO $ atomically $ newTVar nobody
    let glbl = Global pool st
        app = staticApp $ defaultWebAppSettings "web"
        wsMiddle = WS.websocketsOr WS.defaultConnectionOptions (wsHandler glbl Serv.process)

    liftIO
        $ runSettings (setPort 8080 defaultSettings)
        $ (wsMiddle . simpleCors) app

    -- Example TLS version (uncomment WarpTLS above and generate your certs using openssl)
    -- liftIO $ runTLS (tlsSettings "resources/certs/server.crt" "resources/certs/server.key")
    --     { tlsWantClientCert = False
    --     } (setPort 8080 defaultSettings) $ (wsMiddle . simpleCors) app
