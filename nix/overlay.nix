final: previous:
with final.haskell.lib;

{
  linkCheckPackages =
    let
      linkCheckPkg =
        name:
          doBenchmark (
            addBuildDepend (
              failOnAllWarnings (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) {}
                )
              )
            ) (final.haskellPackages.autoexporter)
          );
      linkCheckPkgWithComp =
        exeName: name:
          generateOptparseApplicativeCompletion exeName (linkCheckPkg name);
      linkCheckPkgWithOwnComp = name: linkCheckPkgWithComp name name;
    in
      {
        "linkcheck" = linkCheckPkgWithOwnComp "linkcheck";
      };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (
              old.overrides or (
                _:
                _:
                  {}
              )
            ) (
              self: super:
                final.linkCheckPackages
            );
        }
    );
}
