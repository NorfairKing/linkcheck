final: prev:
with final.haskell.lib;

{
  linkcheck = justStaticExecutables final.haskellPackages.linkcheck;
  haskellPackages =
    prev.haskellPackages.override (old:
      {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: { }))
          (
            self: super:
              {
                linkcheck =
                  generateOptparseApplicativeCompletion "linkcheck" (
                    buildStrictly (
                      disableLibraryProfiling (
                        final.haskellPackages.callCabal2nixWithOptions "linkcheck" (../linkcheck) "--no-hpack" { }
                      )
                    ));
              }
          );
      }
    );
}
