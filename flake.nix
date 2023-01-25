{
  description = "A HerculesCI effect for deploying docs to Github pages";

  outputs = { self }:
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
    };
}
