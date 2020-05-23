{-# LANGUAGE CPP #-}

module SimpleCabal (
  findCabalFile,
  finalPackageDescription,
#if MIN_VERSION_Cabal(2,2,0)
  parseFinalPackageDescription,
#endif
  makeFinalPackageDescription,
  getPackageId,
--  dependencies,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,4,0)
  buildDepends,
#endif
  buildDependencies,
  setupDependencies,
  testsuiteDependencies,

  allBuildInfo,
  BuildInfo (..),
  depPkgName, exeDepName, pkgcfgDepName,
  FlagName, mkFlagName,
  hasExes, hasLibs,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
#else
  licenseFiles,
#endif
  PackageDescription (..),
  PackageIdentifier (..),
  PackageName, mkPackageName, unPackageName,
  packageName, packageVersion,
  readGenericPackageDescription,
  showPkgId,
  showVersion,
  tryFindPackageDesc
  ) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif

#if MIN_VERSION_Cabal(2,2,0)
import qualified Data.ByteString.Char8 as B
#endif
import Data.List (delete, nub)

import Distribution.Compiler
import Distribution.Package  (
                              packageName,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
                              depPkgName,
                              mkPackageName,
                              unPackageName,
                              unPkgconfigName,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,4,0)
                              Dependency,
#endif
#else
#endif
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
                              Dependency (..),
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
                              PackageName,
#else
                              PackageName (..),
#endif
                              PackageIdentifier (..),
                                       )

import Distribution.PackageDescription (
  PackageDescription (..),
  allBuildInfo,
  BuildInfo (..),
--  buildToolDepends,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,4,0)
  enabledBuildDepends,
#endif
  extraLibs,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
  FlagName,
  mkFlagName,
#else
  FlagName (..),
#endif
  GenericPackageDescription(packageDescription),
  hasExes, hasLibs,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
  mkFlagAssignment,
#endif
  pkgconfigDepends,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,24,0)
  setupDepends,
#endif
  targetBuildDepends,
  TestSuite (..)
  )
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
import Distribution.Types.LegacyExeDependency (LegacyExeDependency (..))
import Distribution.Types.PkgconfigDependency (PkgconfigDependency (..))
#else
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
       (readGenericPackageDescription, parseGenericPackageDescriptionMaybe)
#elif defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Parse (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parse (readPackageDescription)
#endif

import Distribution.Simple.Compiler (
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
    compilerInfo
#else
    Compiler (..)
#endif
    )
import Distribution.Simple.Configure (
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
    configCompilerEx
#else
    configCompiler
#endif
    )
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
--import Distribution.Simple.BuildToolDepends (getAllToolDependencies)
import Distribution.Simple.Program   (defaultProgramDb)
#else
import Distribution.Simple.Program   (defaultProgramConfiguration)
#endif
import qualified Distribution.Simple.Utils as DSU (
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
    tryFindPackageDesc
#else
    findPackageDesc
#endif
    )

import Distribution.System (Platform (..), buildArch, buildOS)

import Distribution.Verbosity (normal,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
                               Verbosity
#endif
                              )

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
import Distribution.Pretty (prettyShow)
#else
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.Version (showVersion)
#else
import Data.Version (showVersion)
#endif
#endif

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
import qualified Distribution.Version (Version)
#endif

import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension)

-- | Find the .cabal file in the current directory.
--
-- Errors if more than one or no file found.
--
-- since @0.0.0.1@
findCabalFile :: IO FilePath
findCabalFile = do
  allCabals <- filesWithExtension "." ".cabal"
  case allCabals of
    [file] -> return file
    [] -> error "No .cabal file found"
    _ -> error "More than one .cabal file found!"
  where
    filesWithExtension :: FilePath -> String -> IO [FilePath]
    filesWithExtension dir ext =
      filter (\ f -> takeExtension f == ext && head f /= '.')
      <$> getDirectoryContents dir

-- | Get the package name-version from the .cabal file in the current directory.
--
-- since @0.0.0.1@
getPackageId :: IO PackageIdentifier
getPackageId = do
  gpd <- findCabalFile >>= readGenericPackageDescription normal
  return $ package $ packageDescription gpd

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
readGenericPackageDescription :: Verbosity
                              -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription = readPackageDescription
#endif

#if MIN_VERSION_Cabal(2,2,0)
-- | only available with Cabal-2.2+
parseFinalPackageDescription :: [(FlagName, Bool)] -> B.ByteString
                          -> IO (Maybe PackageDescription)
parseFinalPackageDescription flags cabalfile = do
  let mgenPkgDesc = parseGenericPackageDescriptionMaybe cabalfile
  case mgenPkgDesc of
    Nothing -> return Nothing
    Just genPkgDesc -> Just <$> makeFinalPackageDescription flags genPkgDesc
#endif

-- | Generate PackageDescription from the specified .cabal file and flags.
--
-- since @0.0.0.1@, deprecated in favour of readFinalPackageDescription
finalPackageDescription :: [(FlagName, Bool)] -> FilePath
                          -> IO PackageDescription
finalPackageDescription = readFinalPackageDescription

readFinalPackageDescription :: [(FlagName, Bool)] -> FilePath
                            -> IO PackageDescription
readFinalPackageDescription flags cabalfile =
  readGenericPackageDescription normal cabalfile >>=
  makeFinalPackageDescription flags

makeFinalPackageDescription :: [(FlagName, Bool)] -> GenericPackageDescription
                            -> IO PackageDescription
makeFinalPackageDescription flags genPkgDesc = do
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
  let defaultProgramDb = defaultProgramConfiguration
#endif
  compiler <- do
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,18,0)
    (compiler, _, _) <- configCompilerEx
#else
    (compiler, _) <- configCompiler
#endif
       (Just GHC) Nothing Nothing defaultProgramDb normal
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
    return (compilerInfo compiler)
#else
    return (compilerId compiler)
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
#else
  let mkFlagAssignment = id
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
  let finalizePackageDescription flags' = finalizePD flags' defaultComponentRequestedSpec
#endif
  let final =
        finalizePackageDescription (mkFlagAssignment flags)
        (const True) (Platform buildArch buildOS)
        compiler
        [] genPkgDesc
  case final of
    Left e -> error $ "finalize failed: " ++ show e
    Right res -> return $ fst res

-- | Return the list of build dependencies of a package, excluding itself
buildDependencies :: PackageDescription -> [PackageName]
buildDependencies pkgDesc =
  let deps = nub $ map depPkgName (buildDepends pkgDesc)
      self = pkgName $ package pkgDesc
  in delete self deps

-- | Return the list of testsuite dependencies of a package, excluding itself
testsuiteDependencies :: PackageDescription -> [PackageName]
testsuiteDependencies pkgDesc =
  let self = pkgName $ package pkgDesc in
  delete self . nub . map depPkgName $ concatMap (targetBuildDepends . testBuildInfo) (testSuites pkgDesc)

packageVersion :: PackageIdentifier -> String
packageVersion =
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
  prettyShow . pkgVersion
#else
  showVersion . pkgVersion
#endif

showPkgId :: PackageIdentifier -> String
showPkgId pkgid =
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
  prettyShow pkgid
#else
  unPackageName (packageName pkgid) ++ "-" ++ packageVersion pkgid
#endif

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
#else
unPackageName :: PackageName -> String
unPackageName (PackageName n) = n
#endif

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
showVersion :: Distribution.Version.Version -> String
showVersion = prettyShow
#endif

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
mkFlagName :: String -> FlagName
mkFlagName = FlagName
#endif

tryFindPackageDesc :: FilePath -> IO FilePath
tryFindPackageDesc =
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
  DSU.tryFindPackageDesc
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(3,0,0)
    normal
#endif
#else
  DSU.findPackageDesc
#endif

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
#else
licenseFiles :: PackageDescription -> [FilePath]
licenseFiles pkgDesc =
  [licenseFile pkgDesc | licenseFile pkgDesc /= ""]
#endif

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,4,0)
buildDepends :: PackageDescription -> [Dependency]
buildDepends = flip enabledBuildDepends defaultComponentRequestedSpec
#endif

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
exeDepName :: LegacyExeDependency -> String
exeDepName (LegacyExeDependency n _) = n

pkgcfgDepName :: PkgconfigDependency -> String
pkgcfgDepName (PkgconfigDependency n _) = unPkgconfigName n
#else
depPkgName :: Dependency -> PackageName
depPkgName (Dependency pn _) = pn

exeDepName :: Dependency -> String
exeDepName = unPackageName . depPkgName

pkgcfgDepName :: Dependency -> String
pkgcfgDepName = unPackageName . depPkgName
#endif

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
mkPackageName :: String -> PackageName
mkPackageName = PackageName
#endif

setupDependencies :: PackageDescription  -- ^pkg description
                  -> [PackageName]         -- ^depends
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,24,0)
setupDependencies pkgDesc =
  maybe [] (map depPkgName . setupDepends) (setupBuildInfo pkgDesc)
#else
setupDependencies _pkgDesc = []
#endif

-- dependencies :: PackageDescription  -- ^pkg description
--                 -> ([PackageName], [PackageName], [ExeDependency], [String], [PkgconfigDependency])
--                 -- ^depends, setup, tools, c-libs, pkgcfg
-- dependencies pkgDesc =
--     let --self = pkgName $ package pkgDesc
--         deps = buildDependencies pkgDesc
--         setup = setupDependencies pkgDesc
--         buildinfo = allBuildInfo pkgDesc
--         tools =  nub $ concatMap buildToolDepends buildinfo
--         clibs = nub $ concatMap extraLibs buildinfo
--         pkgcfgs = nub $ concatMap pkgconfigDepends buildinfo
--     in (deps, setup, tools, clibs, pkgcfgs)
