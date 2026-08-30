{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Authorization (AuthAPI, authServer)
import Config 
import Database (createDB)
import JWT (adminAuthHandler, refreshAuthHandler, verifyAccessToken, verifyRefreshToken, createAccessToken, createRefreshToken)
import Portfolio (PortfolioAPI, portfolioServer)
import Tutoring (TutoringAPI, tutoringServer)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.OpenSSL (opensslManagerSettings, withOpenSSL)
import qualified OpenSSL.Session as SSL
import Servant
import Network.Wai.Handler.Warp (run)
import qualified Email
import Logger (newLogger)

-- API type definition

type API =
       "auth" :> AuthAPI
  :<|> "portfolio" :> PortfolioAPI
  :<|> "tutoring" :> TutoringAPI

-- server definition

api :: Proxy API
api = Proxy

main :: IO ()
main = withOpenSSL $ do
    config <- loadConfig

    logger <- newLogger "logs/server.log"

    -- Database pools
    authDB <- createDB (authDbConnString config)
    portfolioDB <- createDB (portfolioDbConnString config)
    portfolioAdminDB <- createDB (portfolioDbAdminConnString config)
    tutoringDB <- createDB (inquiryDbConnString config)

    httpManager <- newManager (opensslManagerSettings SSL.context)

    let createAccessToken' = createAccessToken (jwtSecret config) (domain config)
        createRefreshToken' = createRefreshToken (jwtSecret config) (domain config)
      
        verifyAccessToken' = verifyAccessToken (jwtSecret config) (domain config)
        verifyRefreshToken' = verifyRefreshToken (jwtSecret config) (domain config)

        adminAuthHandler' = adminAuthHandler logger verifyAccessToken'
        refreshAuthHandler' = refreshAuthHandler logger verifyRefreshToken'

        context =
             adminAuthHandler'
          :. refreshAuthHandler'
          :. EmptyContext

        emailSettings =
          Email.EmailSettings
            { Email.settingsManager     = httpManager
            , Email.settingsApiKey      = resendApiKey config
            , Email.settingsFromAddress = fromEmail config
            }

        server =
             authServer logger authDB createAccessToken' createRefreshToken'
          :<|> portfolioServer logger portfolioDB portfolioAdminDB
          :<|> tutoringServer logger tutoringDB emailSettings (notifyEmail config)

    run 8080 $
      serveWithContext api context server