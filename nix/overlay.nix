final: previous:
with final.haskell.lib;

let
  linkcheck =
    generateOptparseApplicativeCompletion "linkcheck" (
      buildStrictly (
        disableLibraryProfiling (
          final.haskellPackages.callCabal2nixWithOptions "linkcheck" (final.gitignoreSource (../linkcheck)) "--no-hpack" { }
        )
      ));
in
{
  linkcheck = justStaticExecutables linkcheck;
  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                { inherit linkcheck; }
            );
      }
    );
}
