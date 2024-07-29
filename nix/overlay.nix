final: prev:
with final.haskell.lib;

{
  linkcheck = justStaticExecutables final.haskellPackages.linkcheck;
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: {
        linkcheck = self.opt-env-conf.installManpagesAndCompletions [ "linkcheck" ] (buildStrictly (self.callPackage ../linkcheck { }));
      }
    );
  }
  );
}
