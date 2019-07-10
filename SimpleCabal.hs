{-# LANGUAGE CPP #-}

module SimpleCabal
       ( findCabalFile
       , finalPackageDescription
       , FlagName, mkFlagName
       , getPackageId
       , normal
       , PackageDescription (..)
       , PackageIdentifier (..)
       , packageName, packageVersion
       , prettyShow
       , readGenericPackageDescription
       , showPkgId
       ) where

#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,8,0))
#else
import Control.Applicative ((<$>))
#endif

import Distribution.Compiler
import Distribution.Package  (PackageIdentifier (..),
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(1,22,0)
                                        unPackageName
#else
                                        PackageName (..)
#endif
                                       )
import Distribution.PackageDescription 
  (PackageDescription (..),
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
  FlagName, 
  mkFlagName,
#else
  FlagName (..),
#endif
  GenericPackageDescription(packageDescription),
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
  mkFlagAssignment
#endif
  )
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.Types.ComponentRequestedSpec (defaultComponentRequestedSpec)
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
