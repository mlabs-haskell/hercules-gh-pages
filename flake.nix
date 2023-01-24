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

      mkGhPagesBuilder = import "${hercules-ci-effects}/effects/gh-pages" {
        inherit pkgs;
        inherit (hci-effects) runIf mkEffect;
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
