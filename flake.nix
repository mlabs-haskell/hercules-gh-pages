{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;
    hercules-ci-effects.url = github:hercules-ci/hercules-ci-effects;
  };

  outputs = { self, nixpkgs, hercules-ci-effects }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      hci-effects = hercules-ci-effects.lib.withPkgs pkgs;

      inherit (nixpkgs) lib;
      inherit (nixpkgs.lib) optionalString;

      # mkGhPagesBuilder ::
      #   { gh-pages :: Derivation
      #   , branchName :: String
      #   , allowedRefs :: [String]
      #   , committer :: { name :: String, email :: String } ? <default value> }
      #   } ->
      #   HerculesCIArgs ->
      #   { onPush.gh-pages.outputs.effects.default :: Effect }
      mkGhPagesBuilder =
        {
          gh-pages,
          branchName ? "gh-pages",
          allowedRefs ? ["refs/heads/main" "refs/heads/master"],
          committer ? {
            name = "Andrey Vlasov";
            email = "andreyvlasov+gh-pages-builder@mlabs.city";
          }
        }:
        { primaryRepo, ... }:
        {
          onPush.gh-pages.outputs.effects.default =
            hci-effects.runIf (lib.elem primaryRepo.ref allowedRefs) (
              hci-effects.mkEffect {
                buildInputs = with pkgs; [ openssh git ];
                secretsMap = {
                  git = { type = "GitToken"; };
                };

                # Env variables
                inherit branchName;
                inherit (primaryRepo) owner remoteHttpUrl;
                committerName = committer.name;
                committerEmail = committer.email;
                githubHostKey = "github.com ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
                ghPages = gh-pages;

                effectScript =
                  ''
                    set -e
                    set -x
                    TOKEN=`readSecretString git .token`
                    ORIGIN=`echo $remoteHttpUrl | sed "s#://#://$owner:$TOKEN@#"`
                    echo githubHostKey >> ~/.ssh/known_hosts
                    export GIT_COMMITTER_NAME="$committerName"
                    export GIT_COMMITTER_EMAIL="$committerEmail"
                    export GIT_AUTHOR_NAME="$committerName"
                    export GIT_AUTHOR_EMAIL="$committerEmail"
                    cp -r --no-preserve=mode $ghPages ./gh-pages && cd gh-pages
                    git init -b $branchName
                    git remote add origin $ORIGIN
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

      herculesCI = mkGhPagesBuilder { inherit (self.packages.${system}) gh-pages; };
    };
}
