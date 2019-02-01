module Main where

-- Let's make a little tool for managing my /etc/hosts file.
-- This could probably be done in a few lines of sed, but whatever. :)

-- I've got several groups of lines in my hosts file that I need to
-- be able to easily turn on and off in different contexts: some
-- for work, some that are time sinks for goofing off, some for ad
-- networks. This tool will automatically comment and uncomment
-- lines by group, where membership in group "FOO" is denoted by
-- putting "<FOO>" at the end of the line.

import Control.Exception
import Control.Monad ((>=>), when)
import Data.List (break, sort, nub, intercalate)
import Data.Maybe
import System.Console.GetOpt
import System.IO
import System.Environment
import System.Exit

-- The `Render` class is for data we want to pretty-print.
class Render t where
  render :: t -> String



-- Is this line commented out or not?
data Status
  = Active | Inactive
  deriving Show

instance Render Status where
  render s = case s of
    Active -> ""
    Inactive -> "#"



-- If a host line doesn't end with a valid group identifier
-- of the form "<FOO>", leave it alone.
data Host = Host
  { hostContent :: String
  , hostGroup   :: Maybe (String, Status)
  } deriving Show

instance Render Host where
  render host = case hostGroup host of
    Nothing -> hostContent host
    Just (g,s) -> concat
      [ render s, hostContent host, "<", g, ">" ]



-- Parses a list of host lines
parseHosts :: [String] -> Either Error [Host]
parseHosts lines = sequence $ zipWith parseHost [1..] lines

-- Parse a single host line. `num` is the line number,
-- for error reporting.
parseHost :: Int -> String -> Either Error Host
parseHost num line = do
  (rest, group) <- stripGroup num line
  case group of
    Nothing -> return $ Host
      { hostContent = line
      , hostGroup = Nothing
      }
    Just g -> do
      (status, content) <- case break (/= '#') rest of
        ([], x) -> Right (Active, x)
        ('#':_,x) -> Right (Inactive, x)
        _ -> Left $ MalformedHostLine num rest
      return $ Host
        { hostContent = content
        , hostGroup = Just (g, status)
        }

-- Strip the group identifier from the end of a host line.
stripGroup :: Int -> String -> Either Error (String, Maybe String)
stripGroup num str = case reverse str of
  '>':rest -> case break (== '<') rest of
    (g,'<':z) -> Right (reverse z, Just $ reverse g)
    _ -> Left $ MalformedGroupName num

  _ -> Right (str, Nothing)



-- Represents parse errors.
data Error
  = MalformedGroupName Int
  | MalformedHostLine Int String
  deriving Show

instance Render Error where
  render err = case err of
    MalformedGroupName k -> concat
      [ "on line " ++ show k ++ ":"
      , "expected '<group-name>' at end of line"
      ]
    MalformedHostLine k str -> concat
      [ "on line " ++ show k ++ ":"
      , "malformed host line\n" ++ str
      ]



-- Next we define operations on hosts and lists of hosts.
-- These will correspond to actions triggered by
-- command line flags.

-- Activate a line if its group is in a given list.
turnOn :: [String] -> Host -> Host
turnOn gs host = case hostGroup host of
  Nothing -> host
  Just (g,_) -> if elem g gs
    then host { hostGroup = Just (g, Active) }
    else host

-- Deactivate a line if its group is in a given list.
turnOff :: [String] -> Host -> Host
turnOff gs host = case hostGroup host of
  Nothing -> host
  Just (g,_) -> if elem g gs
    then host { hostGroup = Just (g, Inactive) }
    else host

-- List all the group names
showGroups :: [Host] -> IO ()
showGroups = mapM_ putStrLn . groups
  where
    groups :: [Host] -> [String]
    groups = nub . sort . map fst . catMaybes . map hostGroup

-- List the host lines having one of the given group names
showHostsByGroup :: [String] -> [Host] -> IO ()
showHostsByGroup gs = mapM_ (putStrLn . render) . filter (groupIn $ nub gs)
  where
    groupIn :: [String] -> Host -> Bool
    groupIn gs host = case hostGroup host of
      Nothing -> False
      Just (g,_) -> elem g gs



-- We'll control execution with an option record.
data Options = Options
  { optHelp :: Bool -- Show usage
  , optVersion :: Bool -- Show version info
  , optVerbose :: Bool -- Chatty output to stderr
  , optDryRun :: Maybe (Either StdOut FilePath) -- Don't overwrite the hosts file
  , optFilePath :: FilePath -- Path to hosts file
  , optTurnOn :: [String] -- Group to activate
  , optTurnOff :: [String] -- Group to deactivate
  , optGroups :: Bool -- List all available group names
  , optList :: Maybe [String] -- List hosts in the given groups
  } deriving Show

data StdOut = StdOut
  deriving (Eq, Show)

defaults :: Options
defaults = Options
  { optHelp = False
  , optVersion = False
  , optVerbose = False
  , optDryRun = Nothing
  , optFilePath = "/etc/hosts"
  , optTurnOn = []
  , optTurnOff = []
  , optGroups = False
  , optList = Nothing
  }



-- We're using the GetOpt package to deal with arguments.
options :: [OptDescr (Options -> Options)]
options =
  [ let
      munge opts = opts { optHelp = True }
    in
      Option ['?'] ["help"] (NoArg munge)
        "show usage"

  , let
      munge opts = opts { optVersion = True }
    in
      Option [] ["version"] (NoArg munge)
        "show version information"

  , let
      munge opts = opts { optVerbose = True }
    in
      Option ['v'] ["verbose"] (NoArg munge)
        "log actions to stderr"

  , let
      munge d opts = case d of
        Nothing -> opts { optDryRun = Just (Left StdOut) }
        Just x -> opts { optDryRun = Just (Right x) }
    in
      Option ['d'] ["dry-run"] (OptArg munge "FILE")
        "write to FILE (stdout)"

  , let
      munge p opts = opts { optFilePath = p }
    in
      Option ['p'] ["path"] (ReqArg munge "FILE")
        "path to hosts file (/etc/hosts)"

  , let
      munge p opts = opts { optTurnOn = p : optTurnOn opts }
    in
      Option ['y'] ["on"] (ReqArg munge "STRING")
        "group name to turn on (multiples allowed)"

  , let
      munge p opts = opts { optTurnOff = p : optTurnOff opts }
    in
      Option ['n'] ["off"] (ReqArg munge "STRING")
        "group name to turn off (multiples allowed)"

  , let
      munge opts = opts { optGroups = True }
    in
      Option ['g'] ["groups"] (NoArg munge)
        "show host groups"

  , let
      munge n opts = case optList opts of
        Nothing -> opts { optList = Just [n] }
        Just ns -> opts { optList = Just (n:ns) }
    in
      Option ['l'] ["list"] (ReqArg munge "STRING")
        "list hosts in group STRING"
  ]



-- Parse the options, reporting any errors.
getOptions :: IO Options
getOptions = do
  argv <- getArgs

  case getOpt Permute options argv of
    -- no errors, no unrecognized options
    (actions, [], []) -> return $ compose actions defaults

    -- unrecognized options
    (_, nonopts, []) -> do
      errPutStrLn "hostgroup: unrecognized option(s)"
      mapM_ errPutStrLn nonopts
      showUsage
      exitFailure

    -- option errors
    (_, _, errs) -> do
      errPutStrLn "hostgroup: option error(s)"
      mapM_ errPutStrLn errs
      showUsage
      exitFailure



-- On with the show
main :: IO ()
main = do
  opts <- getOptions

  -- show usage and quit
  () <- when (True == optHelp opts) $
          showUsage >> exitSuccess

  -- show version info and quit
  () <- when (True == optVersion opts) $
          showVersion >> exitSuccess

  -- read the hosts file
  hostLines <- do
    when (True == optVerbose opts) $
      errPutStrLn $ "Reading from " ++ (optFilePath opts)
    let
      handler :: IOException -> IO a
      handler e = do
        errPutStrLn "hostgroup: Exception!"
        errPutStrLn $ displayException e
        exitFailure
    lines <$> readFile (optFilePath opts) `catch` handler

  -- parse the hosts
  hosts <- case parseHosts hostLines of
    Left err -> do
      errPutStrLn (render err) >> exitFailure
    Right hs -> do
      when (True == optVerbose opts) $
        errPutStrLn $ "Parsed " ++ show (length hs) ++ " lines"
      return hs

  -- show group names and quit
  () <- when (True == optGroups opts) $
          showGroups hosts >> exitSuccess

  -- show hosts with given groups and quit
  () <- case optList opts of
    Nothing -> return ()
    Just gs -> do
      when (True == optVerbose opts) $
        errPutStrLn $ "Showing hosts by group: " ++ intercalate "," gs
      showHostsByGroup gs hosts >> exitSuccess

  let
    alteredHosts =
      map (turnOn $ optTurnOn opts) .
      map (turnOff $ optTurnOff opts) $
      hosts

  let
    outFilePath = case optDryRun opts of
      Nothing -> Just $ optFilePath opts
      Just (Left StdOut) -> Nothing
      Just (Right path) -> Just path

  when (True == optVerbose opts) $ do
    when (Nothing /= optDryRun opts) $
      errPutStrLn "Dry-run mode detected!"
    case outFilePath of
      Nothing -> errPutStrLn "Writing to stdout"
      Just p -> errPutStrLn $ "Writing to " ++ p

  -- write the hosts file
  () <- do
    let
      handler :: IOException -> IO a
      handler e = do
        errPutStrLn "hostgroup: Exception!"
        errPutStrLn $ displayException e
        exitFailure

      contents = unlines $ map render alteredHosts
    case outFilePath of
      Nothing -> putStrLn contents
      Just path -> writeFile path contents `catch` handler

  exitSuccess



-- Like putStrLn, but for stderr.
errPutStrLn :: String -> IO ()
errPutStrLn msg = hPutStr stderr msg >> hPutChar stderr '\n'

showUsage :: IO ()
showUsage = do
  let header = "USAGE: hostgroup [OPTION...]"
  putStrLn $ usageInfo header options

showVersion :: IO ()
showVersion = putStrLn versionString

versionString :: String
versionString = "hostgroup 0.1"

compose :: (Traversable t) => t (a -> a) -> a -> a
compose = foldr (.) id
