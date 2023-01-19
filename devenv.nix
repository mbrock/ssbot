{ pkgs, ... }:

{
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
      zenburn-theme
    ]))
    jq
    ripgrep
    sqlite
    inotify-tools
  ];

  scripts = {
    nodetown.exec = ''
      iex --name nodetown@$(hostname) --cookie nodetown \
        -S mix phx.server
    '';

    nodetown-setup.exec = ''
      mix do local.rebar --force, local.hex --force
      mix escript.install hex livebook
    '';
    
    nodetown-livebook.exec = ''
      export LIVEBOOK_DEFAULT_RUNTIME=attached:nodetown@$(hostname):nodetown
      export LIVEBOOK_PASSWORD=nodetown-$(hostname)
      export LIVEBOOK_IP=0.0.0.0
      livebook server
    '';
  };

  enterShell = ''
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

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";
}
