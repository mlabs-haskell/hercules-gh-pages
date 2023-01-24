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

      inherit (nixpkgs.lib) optionalString;

      # mkGhPagesBuilder ::
      #   { gh-pages :: Derivation
      #   , branchName :: String
      #   , comitter :: { name :: String, email :: String } ? <default value> }
      #   , author :: { name :: String | null, email :: String | null } ? <default value>
      #   } ->
      #   HerculesCIArgs ->
      #   { onPush.gh-pages.outputs.effects.default :: Effect }
      mkGhPagesBuilder =
        {
          gh-pages,
          branchName ? "gh-pages",
          committer ? {
            name = "Andrey Vlasov";
            email = "andreyvlasov+gh-pages-builder@mlabs.city";
          },
          author ? {
            name = "`git log --format=format:%aN -n 1`";
            email = "`git log --format=format:%aE -n 1`";
          }
        }:
        { primaryRepo, ... }:
        {
          onPush.gh-pages.outputs.effects.default =
            hci-effects.runIf (primaryRepo.ref == "refs/heads/main" || primaryRepo.ref == "refs/heads/master") (
              hci-effects.mkEffect {
                buildInputs = with pkgs; [ openssh git ];
                secretsMap = {
                  git = { type = "GitToken"; };
                };
                effectScript =
                  let
                    githubHostKey = "github.com ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
                  in
                  ''
                    set -x
                    echo ${githubHostKey} >> ~/.ssh/known_hosts
                    export GIT_COMMITTER_NAME="${committer.name}"
                    export GIT_COMMITTER_EMAIL="${committer.email}"
                    ${optionalString (author?name && author.name != null)
                      ''
                      export GIT_AUTHOR_NAME="${author.name}"
                      ''}
                    ${optionalString (author?email && author.email != null)
                      ''
                      export GIT_AUTHOR_EMAIL="${author.email}"
                      ''}
                    cp -r --no-preserve=mode ${gh-pages} ./gh-pages && cd gh-pages
                    git init -b ${branchName}
                    git remote add origin https://mlabs-haskell:`readSecretString git .token`@github.com/mlabs-haskell/hercules-gh-pages.git
                    git add .
                    git commit -m "Deploy to ${branchName}"
                    git push -f origin ${branchName}:${branchName}
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
