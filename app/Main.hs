{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Authorization (AuthAPI, authServer)
import Config (loadConfig, jwtSecret, domain)
import JWT (adminAuthHandler, verifyAccessToken)
import Portfolio (PortfolioAPI, portfolioServer)
import Tutoring (TutoringAPI, tutoringServer)
import Servant
import Network.Wai (Application)

-- API type definition

type API =
       "auth" :> AuthAPI
  :<|> "portfolio" :> PortfolioAPI
  :<|> "tutoring" :> TutoringAPI

-- server definition

api :: Proxy API
api = Proxy

server :: Server API
server =
       authServer
  :<|> portfolioServer
  :<|> tutoringServer

main :: IO ()
main = do
    config <- loadConfig

    let verifyAccessToken' =
          verifyAccessToken
            (jwtSecret config)
            (domain config)

        verifyRefreshToken' =
          verifyRefreshToken
            (jwtSecret config)
            (domain config)

        adminAuthHandler' =
          adminAuthHandler verifyAccessToken'

        refreshAuthHandler' =
          refreshAuthHandler verifyRefreshToken'

        context =
             adminAuthHandler'
          :. refreshAuthHandler'
          :. EmptyContext

    run 8080 $
      serveWithContext api context server