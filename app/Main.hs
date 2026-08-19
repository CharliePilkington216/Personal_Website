{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Authorization (AuthAPI, authServer)
import Config 
import Database (createDB)
import JWT (adminAuthHandler, refreshAuthHandler, verifyAccessToken, verifyRefreshToken, createAccessToken, createRefreshToken)
import Portfolio (PortfolioAPI, portfolioServer)
import Tutoring (TutoringAPI, tutoringServer)
import Servant
import Network.Wai.Handler.Warp (run)

-- API type definition

type API =
       "auth" :> AuthAPI
  :<|> "portfolio" :> PortfolioAPI
  :<|> "tutoring" :> TutoringAPI

-- server definition

api :: Proxy API
api = Proxy

main :: IO ()
main = do
    config <- loadConfig

    -- Database pools
    authDB <- createDB (authDbConnString config)
    portfolioDB <- createDB (portfolioDbConnString config)
    tutoringDB <- createDB (inquiryDbConnString config)

    let createAccessToken' = createAccessToken (jwtSecret config) (domain config)
        createRefreshToken' = createRefreshToken (jwtSecret config) (domain config)
      
        verifyAccessToken' = verifyAccessToken (jwtSecret config) (domain config)
        verifyRefreshToken' = verifyRefreshToken (jwtSecret config) (domain config)

        adminAuthHandler' = adminAuthHandler verifyAccessToken'
        refreshAuthHandler' = refreshAuthHandler verifyRefreshToken'

        context =
             adminAuthHandler'
          :. refreshAuthHandler'
          :. EmptyContext

        server =
             authServer authDB createAccessToken' createRefreshToken'
          :<|> portfolioServer portfolioDB
          :<|> tutoringServer tutoringDB

    run 8080 $
      serveWithContext api context server