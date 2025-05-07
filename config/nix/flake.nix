{
  description = "Entorno con herramientas modernas para desarrolladores, Emacs y Qtile";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.ripgrep   # Fast file search
          pkgs.fd        # Fast and user-friendly alternative to find
          pkgs.zellij     # Terminal multiplexer
          pkgs.helix      # Modern text editor
          pkgs.starship   # Minimal, blazing-fast shell prompt
          pkgs.git
          pkgs.neovim
          pkgs.emacs
          pkgs.qtile
        ];

        shellHook = ''
          export PATH="$HOME/.cargo/bin:$PATH"
        '';
      };
    };
}
