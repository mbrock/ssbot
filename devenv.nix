{ pkgs, ... }:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = with pkgs; [ 
    git
    (emacsWithPackages (e: with e; [
      eglot
      elixir-mode
      magit
      marginalia
      nix-mode
      orderless
      paredit
      projectile
      projectile-ripgrep
      typescript-mode
      vertico
      whitespace-cleanup-mode
    ]))
    jq
    git
    ripgrep
  ];

  enterShell = ''
    echo "# welcome to the bot world"
    git --version
    export PATH="$HOME/.mix/escripts:$PATH"
    export EMACSDIR=$(pwd)
  '';

  # https://devenv.sh/languages/
  languages = {
    c.enable = true;
    erlang.enable = true;
    elixir.enable = true;
    elixir.package = pkgs.elixir_1_14;
    javascript.enable = true;
    typescript.enable = true;
    deno.enable = true;
  };

  # https://devenv.sh/scripts/
  # scripts.hello.exec = "echo hello from $GREET";

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";
}
