{ pkgs, config, ... }:

let
  devtools = import ./devtools.nix { inherit pkgs; };
  devcontainer-config = devtools.devcontainer-config {
    vscode-extensions = ["GitHub.copilot" "mkhl.direnv"];
  };

  my-emacs =
    pkgs.emacsWithPackages (e: with e; [
      avy
      company
      consult
      embark
      embark-consult
      marginalia
      orderless
      vertico
      
      default-text-scale
      direnv
      flycheck
      magit
      nix-mode
      paredit
      projectile
      projectile-ripgrep
      rainbow-delimiters
      vterm
      whitespace-cleanup-mode
      zenburn-theme

      ediprolog
      sweeprolog

      # required for copilot.el
      dash s editorconfig
    ]);

  my-swi-prolog =
    (pkgs.swiProlog.override { withGui = true; }).overrideAttrs (old:
      let version = "9.1.7";
      in {
        inherit version;
        src = pkgs.fetchFromGitHub {
          owner = "SWI-Prolog";
          repo = "swipl-devel";
          rev = "V${version}";
          sha256 = "sha256-SwjwDxVNyu8kpbTleiRbsh1/JvrWiRjPhDwnpRmh4GY=";
          fetchSubmodules = true;
        };

        nativeBuildInputs = old.nativeBuildInputs ++ [
          pkgs.ninja
        ];

        buildInputs = old.buildInputs ++ [pkgs.emacs pkgs.pcre2];
      }
    );

  no-mac = pkg:
    if pkgs.stdenv.isDarwin
    then []
    else [pkg];

in {
  # https://devenv.sh/packages/
  packages = with pkgs; (
    no-mac my-swi-prolog
  ) ++ [
    devtools.restless-git
    
#    flyctl
    fx
    git
    graphviz
    imagemagick
    jless
    jq
    ripgrep
    screen
#    sqlite

    esbuild

#    datasette
#    litestream
#    pandoc
#    redis

#    llvmPackages_11.openmp
#    blas
#    faiss

#    lynx
#    links2

#    ffmpeg
#    youtube-dl

    # scryer-prolog

#    elixir-version
#    (elixir_ls.override { elixir = elixir-version; })

    my-emacs

#    (texlive.combine {
#      inherit (texlive)
#        scheme-basic
#        ebgaramond
#        etoolbox
#        extsizes
#        parskip
#        geometry
#        crop
#        titlesec
#        xkeyval
#        fontaxes
#        dvipng
#      ;
#    })
  ];

  scripts = {
    nodetown-tailscale.exec = ''
      sudo tailscale up --accept-routes
    '';

    nodetown.exec = ''
      swipl 
    '';

    gensym.exec = ''
      cat /dev/urandom | tr -dc 'a-z' | fold -w 8 | head -n 1
    '';

    nodetown-remote.exec = ''
      set -x
      iex --sname $(gensym) --cookie nodetown \
        --remsh nodetown@$(hostname)
    '';

    nodetown-eval.exec = ''
      iex --sname $(gensym) --cookie nodetown \
        --remsh nodetown@$(hostname) --eval "$@"
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

    export VAULT_ADDR=http://hamlet:8200/

#    jq < ${devcontainer-config} \
#       > ${config.env.DEVENV_ROOT}/.devcontainer.json

    ${if pkgs.stdenv.isDarwin
      then ""
      else "export LD_PRELOAD=${my-swi-prolog}/lib/libswipl.so"}
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

  processes.nodetown-vnc.exec =
    let
      width = 1400;
      height = 900;
      sizeString = "${toString width}x${toString height}x16";
    in ''
      set -ex
      export DISPLAY=:1

      ${pkgs.xvfb-run}/bin/xvfb-run -n 1 -s "-screen 0 ${sizeString}" \
        ${pkgs.dbus.dbus-launch} --exit-with-session \
          ${pkgs.openbox}/bin/openbox &

      sleep 3
      ${pkgs.xterm}/bin/xterm &

      ${pkgs.x11vnc}/bin/x11vnc -forever -shared -quiet -display :1 \
        -noxrecord -xkb -passwd nodetown &

      wait
    '';
}
