{ pkgs, ... }:

let
  elixir-version = pkgs.elixir_1_14;
in {
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
      vterm
    ]))
    inotify-tools
    jq
    ripgrep
    screen
    sqlite

    elixir-version
    (elixir_ls.override { elixir = elixir-version; })
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
    javascript.enable = true;
    typescript.enable = true;
    deno.enable = true;
  };

  # https://devenv.sh/pre-commit-hooks/
  # pre-commit.hooks.shellcheck.enable = true;

  # https://devenv.sh/processes/
  # processes.ping.exec = "ping example.com";
}
