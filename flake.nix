{
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
            inherit (final) lib mkEffect runIf;
            inherit (final.lib) optionalString;
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
              buildInputs = with pkgs; [ openssh git ];
              secretsMap = {
                git = { type = "GitToken"; };
              };

              # Env variables
              inherit branchName;
              inherit (primaryRepo) owner remoteHttpUrl;
              ghPages = gh-pages;
              GIT_COMMITTER_NAME = committer.name;
              GIT_COMMITTER_EMAIL = committer.email;
              GIT_AUTHOR_NAME = committer.name;
              GIT_AUTHOR_EMAIL = committer.email;
              inherit rewriteHistory;

              effectScript =
                ''
                  set -e
                  TOKEN=`readSecretString git .token`
                  ORIGIN=`echo $remoteHttpUrl | sed "s#://#://$owner:$TOKEN@#"`
                  if [[ $rewriteHistory -eq 1 ]]
                  then
                    mkdir ./gh-pages
                    cd gh-pages
                    git init --initial-branch $branchName
                    git remote add origin $ORIGIN
                  else
                    git clone --branch $branchName --single-branch $ORIGIN gh-pages
                    cd gh-pages
                  fi
                  rm -r *
                  cp -r --no-preserve=mode -T $ghPages .
                  if [ -n "`git status --porcelain`" ]
                  then
                    git add .
                    git commit -m "Deploy to $branchName"
                    git push -f origin $branchName:$branchName
                  else
                    echo "Nothing to commit"
                  fi
                '';
            }
          );
      };

      packages.${system}.gh-pages = pkgs.runCommandNoCC "generate-gh-pages" { }
        ''
          mkdir $out
          echo "<h1>This is a GH page</h1>" > $out/index.html
        '';

      herculesCI = pkgs.mkGhPagesJob { inherit (self.packages.${system}) gh-pages; rewriteHistory = false; };
    };
}
