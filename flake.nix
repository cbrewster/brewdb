{
  inputs = {
    utils.url = "github:numtide/flake-utils";

    zig-overlay = {
      url = "github:mitchellh/zig-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    zls-flake = {
      url = "github:zigtools/zls";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.zig-overlay.follows = "zig-overlay";
    };
  };
  outputs = { nixpkgs, utils, zig-overlay, zls-flake, ... }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; };
      zig = zig-overlay.packages.${system}."0.15.1";
      zls = zls-flake.packages.${system}.default;
    in
    {
      devShell = pkgs.mkShell {
        buildInputs = [
          zls
          zig
        ] ++ (with pkgs; [
          aflplusplus
          linuxPackages.perf
          valgrind
          flamegraph
        ]);
      };
    }
  );
}
