{ pkgs, config, ... }:

let
  elixir-version = pkgs.elixir_1_14;
  devtools = import ./devtools.nix { inherit pkgs; };
  devcontainer-config = devtools.devcontainer-config {
    vscode-extensions = [
      "GitHub.copilot"
      "JakeBecker.elixir-ls"
      "phoenixfoundation.phoenix"
      "mkhl.direnv"
    ];
  };
in {
  # https://devenv.sh/packages/
  packages = with pkgs; [
    devtools.restless-git
    
    screen
    inotify-tools
    
    jq
    ripgrep
    
    git
    sqlite
    datasette
    litestream
    pandoc

    llvmPackages_11.openmp
    blas
    faiss

    lynx
    links2

    ffmpeg
    youtube-dl

    elixir-version
    (elixir_ls.override { elixir = elixir-version; })
    
    (emacsWithPackages (e: with e; [
      company
      datasette
      default-text-scale
      direnv
      eglot
      elixir-mode
      flycheck
      magit
      marginalia
      nix-mode
      orderless
      paredit
      projectile
      projectile-ripgrep
      rainbow-delimiters
      typescript-mode
      vertico
      vterm
      whitespace-cleanup-mode
      zenburn-theme
      zig-mode
    ]))

    (texlive.combine {
      inherit (texlive)
        scheme-basic
        ebgaramond
        etoolbox
        extsizes
        parskip
        geometry
        crop
        titlesec
        xkeyval
        fontaxes
        dvipng
      ;
    })
  ];

  scripts = {
    nodetown-tailscale.exec = ''
      sudo tailscale up --accept-routes
    '';
    
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

    nodetown-datasette.exec = ''
      datasette --host 0.0.0.0 nodetown_dev.db
    '';
  };

  enterShell = ''
    export PATH="$HOME/.mix/escripts:$PATH"
    export EMACSDIR=$(pwd)
    export TERM=xterm-256color
    
    # This was necessary to install ExFaiss.
    export CPLUS_INCLUDE_PATH="$C_INCLUDE_PATH"

    export TERMINUSDB_DOCKER=docker
    export TERMINUSDB_PASS=nodetown
#    export TERMINUSDB_CONTAINER=terminusdb
    export TERMINUSDB_SERVER_IP=0.0.0.0
    export TERMINUSDB_AUTOLOGIN_ENABLED=false

    jq < ${devcontainer-config} \
       > ${config.env.DEVENV_ROOT}/.devcontainer.json
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

  processes.code-server.exec = ''
    ${pkgs.code-server}/bin/code-server --bind-addr 127.0.0.1:5000
  '';
}
