{ pkgs, config, ... }:

let
  devtools = import ./devtools.nix { inherit pkgs; };

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
      # sweeprolog

      # required for copilot.el
      dash s editorconfig
    ]);

  swiProlog = {
    version = "9.1.7";
    src = pkgs.fetchFromGitHub {
      owner = "SWI-Prolog";
      repo = "swipl-devel";
      rev = "V${swiProlog.version}";
      sha256 = "sha256-SwjwDxVNyu8kpbTleiRbsh1/JvrWiRjPhDwnpRmh4GY=";
      fetchSubmodules = true;
    };
  };

  my-swi-prolog =
    (pkgs.swiProlog.override { withGui = true; }).overrideAttrs
      (old: {
        inherit (swiProlog) src version;

        nativeBuildInputs = old.nativeBuildInputs ++ [pkgs.ninja];
        buildInputs = old.buildInputs ++ [pkgs.emacs pkgs.pcre2];
      });

in {
  packages = [
    my-swi-prolog
    my-emacs
    devtools.restless-git
  ] ++ (with pkgs; [
    esbuild
    fx
    git
    graphviz
    imagemagick
    jless
    jq
    ripgrep
    tmux
    scryer-prolog
    redis
  ]);

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

    export LD_PRELOAD=${my-swi-prolog}/lib/libswipl.so
    export SWI_SOURCE=${swiProlog.src}
  '';

  scripts = {
    nodetown-tailscale.exec = ''
      sudo tailscale up --accept-routes
    '';

    nodetown.exec = ''
      swipl -g "consult(index), module(nt), site, dial"
    '';
  };

  languages = {
    c.enable = true;
    javascript.enable = true;
    typescript.enable = true;
    deno.enable = true;
  };

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
