{-# LANGUAGE CPP #-}

module SimpleCabal (
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,4,0)
  buildDepends,
#endif
  exeDepName, pkgcfgDepName,
  findCabalFile,
  finalPackageDescription,
  FlagName, mkFlagName,
  getPackageId,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
#else
  licenseFiles,
#endif
  normal,
  PackageDescription (..),
  PackageIdentifier (..),
  PackageName, depPkgName, mkPackageName, unPackageName,
  packageName, packageVersion,
  prettyShow,
  readGenericPackageDescription,
  setupDependencies,
  showPkgId,
  tryFindPackageDesc
  ) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif

import Distribution.Compiler
import Distribution.Package  (
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
                              unPackageName,
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
                              depPkgName,
                              mkPackageName,
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
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,4,0)
  enabledBuildDepends,
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
  FlagName, 
  mkFlagName,
#else
  FlagName (..),
#endif
  GenericPackageDescription(packageDescription),
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
  mkFlagAssignment,
#endif
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,24,0)
  setupDepends
#endif
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
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
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
import Distribution.Simple.Program   (defaultProgramDb)
#else
import Distribution.Simple.Program   (defaultProgramConfiguration)
#endif
import Distribution.Simple.Utils (
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

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
import Distribution.Pretty (prettyShow)
#else
import qualified Distribution.Version (showVersion, Version)
#endif
#else
import qualified Data.Version (
    showVersion,
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
    Version,
#else
    Version(..)
#endif
  )
#endif

import System.Directory (getDirectoryContents)
import System.FilePath (takeExtension)

type Flags = [(FlagName, Bool)]

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
      filter (\ f -> takeExtension f == ext) <$> getDirectoryContents dir

getPackageId :: IO PackageIdentifier
getPackageId = do
  gpd <- findCabalFile >>= readGenericPackageDescription normal
  return $ package $ packageDescription gpd

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
readGenericPackageDescription :: Distribution.Verbosity.Verbosity
                              -> FilePath -> IO GenericPackageDescription
readGenericPackageDescription = readPackageDescription
#endif

finalPackageDescription :: Flags -> FilePath
                          -> IO PackageDescription
finalPackageDescription flags cabalfile = do
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
  let defaultProgramDb = defaultProgramConfiguration
#endif
  genPkgDesc <- readGenericPackageDescription normal cabalfile
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

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
#else
unPackageName :: PackageName -> String
unPackageName (PackageName n) = n
#endif

packageName :: PackageIdentifier -> String
packageName = unPackageName . pkgName

packageVersion :: PackageIdentifier -> String
packageVersion = prettyShow . pkgVersion

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
#else
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
prettyShow :: Distribution.Version.Version -> String
prettyShow = Distribution.Version.showVersion
#else
prettyShow :: Data.Version.Version -> String
prettyShow = Data.Version.showVersion
#endif
#endif

showPkgId :: PackageIdentifier -> String
showPkgId pkgid =
  packageName pkgid ++ "-" ++ packageVersion pkgid

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
#else
mkFlagName :: String -> FlagName
mkFlagName = FlagName
#endif

#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,20,0)
#else
tryFindPackageDesc :: FilePath -> IO FilePath
tryFindPackageDesc = findPackageDesc
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
pkgcfgDepName = unPackageName. depPkgName
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
