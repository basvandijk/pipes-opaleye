{ mkDerivation, aeson, attoparsec, base, base16-bytestring
, bytestring, case-insensitive, containers, contravariant, dotenv
, hspec, hspec-discover, multiset, postgresql-simple, pretty
, product-profunctors, profunctors, QuickCheck, semigroups, stdenv
, text, time, time-locale-compat, transformers, uuid, void
}:
mkDerivation {
  pname = "opaleye";
  version = "0.5.4.0";
  sha256 = "0dyvaci8dpd5rnr40ib1al2mw2ivza02wbsdz1m5qc7hn30374yv";
  libraryHaskellDepends = [
    aeson attoparsec base base16-bytestring bytestring case-insensitive
    contravariant postgresql-simple pretty product-profunctors
    profunctors semigroups text time time-locale-compat transformers
    uuid void
  ];
  doCheck = false;
  testHaskellDepends = [
    aeson base containers contravariant dotenv hspec hspec-discover
    multiset postgresql-simple product-profunctors profunctors
    QuickCheck semigroups text time transformers
  ];
  homepage = "https://github.com/tomjaguarpaw/haskell-opaleye";
  description = "An SQL-generating DSL targeting PostgreSQL";
  license = stdenv.lib.licenses.bsd3;
}
