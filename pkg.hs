import Control.Exception (SomeException, catch)
import System.Process (callCommand)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (unwords)
import Data.Maybe (fromMaybe)

usage = putStrLn "USAGE:\n\
  \pkg <install|remove> package1 package2 ...\n\
  \pkg <sync|update>\n"

cmd :: [String] -> Maybe String -> Maybe String -> IO()
cmd args maybeSucmd maybeCmdName = do
  let command = sucmd ++ cmdName ++ ' ' : unwords args where
      sucmd = fromMaybe "" $ fmap (++ " ") maybeSucmd
      cmdName = fromMaybe "pacman" maybeCmdName
  catch (callCommand command) handleError

handleError :: SomeException -> IO()
handleError e = do
  putStrLn $ show (e :: SomeException)
  exitFailure

pkg :: [String] -> IO ()
pkg args = case args of
  ["sync"] -> cmd ["-Sy"] (Just "sudo") Nothing
  ["sync", "-force"] -> cmd ["-Syy"] (Just "sudo") Nothing

  ["update"] -> cmd ["-Su"] (Just "sudo") Nothing
  ["update", "-sync"] -> cmd ["-Syyu"] (Just "sudo") Nothing
  ["update", "-aur"] -> cmd ["-Aucax"] (Just "sudo") (Just "aura")

  ["upgrade"] -> cmd ["-Su"] (Just "sudo") Nothing
  ["upgrade", "-sync"] -> cmd ["-Syyu"] (Just "sudo") Nothing
  ["upgrade", "-aur"] -> cmd ["-Aucax"] (Just "sudo") (Just "aura")

  ["install", "-help"] -> usage
  "install" : "-y" : rest -> cmd ("--noconfirm" : "-S" : rest) (Just "sudo") Nothing
  "install" : "-aur" : "-edit" : rest -> cmd ("-Acax --hotedit" : rest) (Just "sudo") (Just "aura")
  "install" : "-aur" : rest -> cmd ("-Acax" : rest) (Just "sudo") (Just "aura")
  "install" : rest -> cmd ("-S" : rest) (Just "sudo") Nothing

  ["remove", "-help"] -> usage
  ["remove", "-orphan"] -> cmd ["-Oj"] (Just "sudo") (Just "aura")
  "remove" : "-all" : rest -> cmd ("-Runsc" : rest) (Just "sudo") Nothing
  "remove" : rest -> cmd ("-Rs" : rest) (Just "sudo") Nothing

  ["list", "-help"] -> usage
  ["list", "-installed"] -> cmd ["-Q"] Nothing Nothing
  ["list", "-installed", "-only-names"] -> cmd ["-Qq"] Nothing Nothing
  "list" : "-group" : rest -> cmd  ("-Qs" : rest) Nothing Nothing

  ["search", "-help"] -> usage
  "search" : "-installed" : rest -> cmd ("-Q" : rest) Nothing Nothing
  "search" : "-aur" : rest -> cmd ("-As" : rest) Nothing (Just "aura")
  "search" : rest -> cmd ("-Ss" : rest) Nothing Nothing

  ["info", "-help"] -> usage
  "info" : "-aur" : rest -> cmd ("-Ai" : rest) Nothing Nothing
  "info" : rest -> cmd ("-Si" : rest) Nothing Nothing

  ["show", "-help"] -> usage
  ["show", "-orphan"] -> cmd ["-O"] Nothing (Just "aura")
  "show" : "-aur" : "-deps" : rest -> cmd ("-Ad" : rest) Nothing (Just "aura")

  [] -> usage
  _ -> putStrLn "Error: Invalid command"


main :: IO ()
main = do
  args <- getArgs
  pkg args

