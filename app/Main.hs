{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import           Control.Monad.IO.Class   (liftIO)
import           Control.Monad.Reader     (ReaderT, runReaderT)
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.List                (intersperse)
import           GHC.Generics             (Generic)
import           Network.Wai.Handler.Warp (run)
import           Servant                  ((:<|>) (..), (:>), Context (..), Get,
                                           Handler, Header, Headers,
                                           IsSecure (..), JSON, NoContent (..),
                                           PlainText, PostNoContent,
                                           Proxy (Proxy), ReqBody, ServerT,
                                           err401, hoistServerWithContext,
                                           serveWithContext, throwError)
import           Servant.Auth.Server      (Auth, AuthResult (..), Cookie (..),
                                           CookieSettings (..), FromJWT,
                                           JWTSettings, SetCookie, ToJWT,
                                           acceptLogin, clearSession,
                                           defaultCookieSettings,
                                           defaultJWTSettings, generateKey,
                                           throwAll)

data AuthData = AuthData
              { name :: String
              }
    deriving (Read, Show, Eq, Generic)

instance FromJSON AuthData
instance ToJSON AuthData
instance FromJWT AuthData
instance ToJWT AuthData

data Session = Session
          { user :: String
          }
    deriving (Read, Show, Eq, Generic)

instance FromJSON Session
instance ToJSON Session
instance FromJWT Session
instance ToJWT Session

data AppConfig = AppConfig
               { appName :: String
               }
    deriving (Read, Show, Eq, Generic)

type App = ReaderT AppConfig Handler

main :: IO ()
main = do
    jwtCfg <- defaultJWTSettings <$> generateKey
    run 8080 $ serveWithContext apiProxy (mkCtx jwtCfg)
             $ hoistServerWithContext apiProxy
                                      ctxProxy
                                      (flip runReaderT appConfig)
             $ server cookieCfg jwtCfg
  where
    cookieCfg    = defaultCookieSettings{ cookieIsSecure = NotSecure
                                        , cookieXsrfSetting = Nothing
                                        }
    apiProxy     = Proxy :: Proxy (API '[Cookie])
    ctxProxy     = Proxy :: Proxy '[CookieSettings, JWTSettings]
    mkCtx jwtCfg = cookieCfg :. jwtCfg :. EmptyContext
    appConfig    = AppConfig "The fancy webapp!"

type API auths       = Signin :<|> Protected auths
type Signin          = "signin"
                    :> ReqBody '[JSON] AuthData
                    :> PostNoContent '[JSON] AuthResponse
type Protected auths = Auth auths Session :>
                     ( "hello"  :> Get '[PlainText] String
                  :<|> "info"   :> Get '[PlainText] String
                  :<|> "signoff" :> PostNoContent '[JSON] AuthResponse
                     )
type AuthResponse    = Headers '[CookieHeader, CookieHeader] NoContent
type CookieHeader    = Header "Set-Cookie" SetCookie

server :: CookieSettings
       -> JWTSettings
       -> ServerT (API auths) App
server cookieCfg jwtCfg = signinHandler cookieCfg jwtCfg
                     :<|> protectedHandlers
  where protectedHandlers (Authenticated user) = helloHandler user
                                            :<|> infoHandler user
                                            :<|> signoffHandler cookieCfg user
        protectedHandlers _                    = throwAll err401

signinHandler :: CookieSettings
              -> JWTSettings
              -> AuthData
              -> App AuthResponse
signinHandler cookieCfg jwtCfg authData =
    (fmap (<*>)) (liftIO $ acceptLogin cookieCfg jwtCfg $ toSession authData)
             <*> (return $ return NoContent)
             >>= maybe (throwError err401) return
  where
    toSession = Session . name

signoffHandler :: CookieSettings
               -> Session
               -> App AuthResponse
signoffHandler cookieCfg _ = return $ clearSession cookieCfg NoContent

helloHandler :: Session
             -> App String
helloHandler (Session user) =
    mkMessageHandler ["Hello", user, ", you are a member of an elite society."]

infoHandler :: Session
            -> App String
infoHandler (Session user) =
    mkMessageHandler ["Your name consits of", show $ length user, "characters."]

mkMessageHandler :: [String]
                 -> App String
mkMessageHandler = return . concat . intersperse " "
