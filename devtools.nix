{ pkgs }:

{
  restless-git = pkgs.symlinkJoin {
    name = "restless-git";
    paths = [
      (pkgs.writeShellScriptBin "git-save" ''
        git add -A
        git diff HEAD --quiet || git summary | git commit -F-
      '')

      (pkgs.writeShellScriptBin "git-summary" ''
        echo -n "<`whoami`> "
        if test `git status --short | wc -l` = 1
        then echo `git status --short` `git shortstat | sed 's,1 file changed ,,'`
        else git shortstat
        fi
      '')

      (pkgs.writeShellScriptBin "git-shortstat" ''
        git diff HEAD --shortstat | cut -c2- | sed s/,//g |
        sed -E 's/([0-9]+) insertions?\(\+\)/+\1/' |
        sed -E 's/([0-9]+) deletions?\(-\)/-\1/' |
        sed -E 's/\+([0-9]+) -([0-9]+)/+\1\/-\2/' |
        sed -E 's/([^ ]+)$/(\1)/'
      '')
    ];
  };

  devcontainer-config = { vscode-extensions }:
    pkgs.writeText "devcontainer.json" (
      builtins.toJSON {
        image = "ghcr.io/cachix/devenv:latest";
        overrideCommand = false;
        customizations.vscode.extensions = vscode-extensions;
      }
    );
}
