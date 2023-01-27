{
  description = "A HerculesCI effect for deploying docs to Github pages";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    hercules-ci-effects.url = github:hercules-ci/hercules-ci-effects;
  };

  outputs = { self, nixpkgs, hercules-ci-effects }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; overlays = [ hercules-ci-effects.overlay self.overlays.default ]; };
    in
    {
      overlays.default = final: prev: {
        mkGhPagesJob = config: herculesEnv: {
          onPush.gh-pages.outputs.effects.default = final.mkGhPagesEffect config herculesEnv;
        };
        mkGhPagesEffect =
          let
            inherit (final) lib;
            inherit (final.lib) optionalString;
            inherit (final.effects) mkEffect runIf;
          in
          { gh-pages
          , branchName ? "gh-pages"
          , condition ? { ref, ... }: lib.elem ref [ "refs/heads/main" "refs/heads/master" ]
          , committer ? {
              name = "Andrey Vlasov";
              email = "andreyvlasov+gh-pages-builder@mlabs.city";
            }
          , rewriteHistory ? true
          }:
          { primaryRepo, ... }:
          runIf (condition primaryRepo) (
            mkEffect {
              buildInputs = with final; [ openssh git ];
              secretsMap = {
                git = { type = "GitToken"; };
              };

              # Env variables
              inherit branchName gh-pages rewriteHistory;
              inherit (primaryRepo) owner remoteHttpUrl;
              GIT_COMMITTER_NAME = committer.name;
              GIT_COMMITTER_EMAIL = committer.email;
              GIT_AUTHOR_NAME = committer.name;
              GIT_AUTHOR_EMAIL = committer.email;

              effectScript = final.writers.writeHaskell
                "effect.hs"
                { libraries = with final.haskellPackages; [aeson modern-uri turtle]; }
                ./effect.hs;
            }
          );
      };

      packages.${system}.gh-pages = pkgs.runCommandNoCC "generate-gh-pages" { }
        ''
          mkdir $out
          echo "<h1>This is a GH page (ed. 3)</h1>" > $out/index.html
        '';

      herculesCI = pkgs.mkGhPagesJob {
        inherit (self.packages.${system}) gh-pages;
        rewriteHistory = false;
        condition = _: true;
      };
    };
}
