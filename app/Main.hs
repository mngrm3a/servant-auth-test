{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson
import           GHC.Generics             (Generic)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Auth.Server

main :: IO ()
main = do
    jwtCfg <- defaultJWTSettings <$> generateKey
    run 8080 $ serveWithContext api (cookieCfg :. jwtCfg :. EmptyContext)
             $ server cookieCfg jwtCfg
  where
    cookieCfg = defaultCookieSettings{ cookieIsSecure = NotSecure
                                     , cookieXsrfSetting = Nothing
                                     }

api                     = Proxy :: Proxy (API '[Cookie])
server cookieCfg jwtCfg = privateHandler :<|> publicHandler cookieCfg jwtCfg

type API auths = Private auths :<|> Public
type Public        =
    "login" :> ReqBody '[JSON] User :> PostNoContent '[JSON] AuthResponse
type Private auths = Auth auths User :> "private" :> Get '[PlainText] String
type AuthResponse  = Headers '[CookieHeader, CookieHeader] NoContent
type CookieHeader  = Header "Set-Cookie" SetCookie

data User = User
          { name :: String
          }
    deriving (Read, Show, Eq, Generic)

instance FromJSON User
instance ToJSON User
instance FromJWT User
instance ToJWT User

publicHandler :: CookieSettings -> JWTSettings -> User -> Handler AuthResponse
publicHandler cookieCfg jwtCfg user = do
    cookieFn <- liftIO $ mkCookieFn
    case cookieFn <*> (pure NoContent) of
        Just r  -> return r
        Nothing -> throwError err401
  where
    mkCookieFn = acceptLogin cookieCfg jwtCfg user

privateHandler :: AuthResult User -> Handler String
privateHandler (Authenticated (User n)) =
    return $ n ++ ", you are a member of an elite society.\n"
privateHandler _                        = throwError err401
