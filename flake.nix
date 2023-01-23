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
    in
    {
      packages.${system}.gh-pages = pkgs.runCommandNoCC "generate-gh-pages" {}
        ''
          mkdir $out
          echo "<h1>This is a GH page</h1>" > $out/index.html
        '';

      effects = src: {
        gh-pages = hci-effects.runIf (src.ref == "refs/heads/main" || src.ref == "refs/heads/master") (
          hci-effects.mkEffect {
            buildInputs = with pkgs; [ openssh git ];
            secretsMap = {
              "ssh" = "ssh";
            };
            effectScript =
              let
                githubHostKey = "github.com ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAq2A7hRGmdnm9tUDbO9IDSwBK6TbQa+PXYPCPy6rbTrTtw7PHkccKrpp0yVhp5HdEIcKr6pLlVDBfOLX9QUsyCOV0wzfjIJNlGEYsdlLJizHhbn2mUjvSAHQqZETYP81eFzLQNnPHt4EVVUh7VfDESU84KezmD5QlWpXLmvU31/yMf+Se8xhHTvKSCZIFImWwoG6mbUoWf9nzpIoaSjB+weqqUUmpaaasXVal72J+UX2B+2RPW3RcT0eOzQgqlJL3RKrTJvdsjE3JEAvGq3lGHSZXy28G3skua2SmVi/w4yCE6gbODqnTWlg7+wC604ydGXA8VJiS5ap43JXiUFFAaQ==";
              in
              ''
                writeSSHKey
                echo ${githubHostKey} >> ~/.ssh/known_hosts
                export GIT_AUTHOR_NAME="Hercules-CI Effects"
                export GIT_COMMITTER_NAME="Hercules-CI Effects"
                export EMAIL="github@croughan.sh"
                cp -r --no-preserve=mode ${self.packages.${system}.gh-pages} ./gh-pages && cd gh-pages
                git init -b gh-pages
                git remote add origin git@github.com:Plutonomicon/plutonomicon.git
                git add .
                git commit -m "Deploy to gh-pages"
                git push -f origin gh-pages:gh-pages
              '';
          }
        );
      };
    };
}
