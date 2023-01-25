# `gh-pages` effect

`gh-pages` is an effect intended to be used to build documentation and deploy it to `gh-pages` branch.

## Example

```nix
{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    hercules-ci-effects.url = github:hercules-ci/hercules-ci-effects;
    hercules-gh-pages.url = github:mlabs-haskell/hercules-gh-pages;
  };

  outputs = { self, nixpkgs, hercules-ci-effects, hercules-gh-pages }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ hercules-ci-effects.overlay hercules-gh-pages.overlays.default ];
      };
    in
    {
      # example "documentation"
      packages.${system}.gh-pages = pkgs.runCommandNoCC "generate-gh-pages" {}
        ''
          mkdir $out
          echo "<h1>This is a GH page</h1>" > $out/index.html
        '';

      # consise way to get your gh-pages job visible to Hercules CI
      herculesCI = pkgs.mkGhPagesJob { inherit (self.packages.${system}) gh-pages; };
    };
}
```

In case you already have jobs:

```nix
herculesCI = herculesEnv: {
  onPush.myJob = ...;
  onPush.gh-pages.outputs.effects.default =
    # note that we use mkGhPages_Effect_ here
    pkgs.mkGhPagesEffect
      { inherit (self.packages.${system}) gh-pages; }
      herculesEnv;
};
```

If you only have `herculesCI.ciSystems = ...;` and wondering what is this `herculesEnv`:

```diff
-herculesCI.ciSystems = ...;
+herculesCI = herculesEnv: {
+  ciSystems = ...;
+  pkgs.mkGhPagesEffect
+    { inherit (self.packages.${system}) gh-pages; }
+    herculesEnv;
+};
```

## Reference

```PureScript
type GhPagesConfig =
  { gh-pages :: Derivation
      -- derivation to build and push
  , branchName :: String ? "gh-pages"
      -- branch to push
  , condition :: Repository -> Boolean ? { ref, ... }: lib.elem ref ["refs/heads/main" "refs/heads/master"]
      -- if a repository does not satisfy this condition, the effect will not be run
      -- by default it checks that CI is triggered by main branch
      -- Repository is the type of https://docs.hercules-ci.com/hercules-ci-agent/evaluation#param-herculesCI-primaryRepo
  , committer :: { name :: String, email :: String } ? <default: credentials of sincerely yours> }
      -- name and email that will be used as author and committer by Git
      -- you probably want to change these
  , rewriteHistory :: Boolean ? true
      -- if true, script resets history on target branch to a single (newest) commit
      -- if you need history preserved, change this to false
  }

pkgs.mkGhPagesEffect ::
  GhPagesConfig ->
  HerculesCIArgs ->
  Effect

pkgs.mkGhPagesJob ::
  GhPagesConfig ->
  HerculesCIArgs ->
  { onPush.gh-pages.outputs.effects.default :: Effect }
```
