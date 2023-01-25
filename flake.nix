{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    hercules-ci-effects.url = github:Mr-Andersen/hercules-ci-effects;
  };

  outputs = { self, nixpkgs, hercules-ci-effects }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      hci-effects = hercules-ci-effects.lib.withPkgs pkgs;

      # mkGhPagesBuilder = import "${hercules-ci-effects}/effects/gh-pages" {
      #   inherit pkgs;
      #   inherit (hci-effects) runIf mkEffect;
      # };

      inherit (pkgs) lib;
      inherit (pkgs.lib) optionalString;
      inherit (hci-effects) mkEffect runIf;

      mkGhPagesBuilder =
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
        {
          onPush.gh-pages.outputs.effects.default =
            runIf (condition primaryRepo) (
              mkEffect {
                buildInputs = with pkgs; [ openssh git ];
                secretsMap = {
                  git = { type = "GitToken"; };
                };

                # Env variables
                inherit branchName;
                inherit (primaryRepo) owner remoteHttpUrl;
                githubHostKey = "github.com ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
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
                    echo githubHostKey >> ~/.ssh/known_hosts
                    if [[ $rewriteHistory -eq 1 ]]
                    then
                      mkdir ./gh-pages
                      cd gh-pages
                      git init --initial-branch $branchName
                      git remote add origin $ORIGIN
                    else
                      git clone --branch $branchName $ORIGIN
                      cd gh-pages
                    fi
                    cp -r --no-preserve=mode $ghPages .
                    git add .
                    git commit -m "Deploy to $branchName"
                    git push -f origin $branchName:$branchName
                  '';
              }
            );
        };
    in
    {
      lib = { inherit mkGhPagesBuilder; };

      packages.${system}.gh-pages = pkgs.runCommandNoCC "generate-gh-pages" {}
        ''
          mkdir $out
          echo "<h1>This is a GH page</h1>" > $out/index.html
        '';

      herculesCI = mkGhPagesBuilder { inherit (self.packages.${system}) gh-pages; rewriteHistory = false; };
    };
}
