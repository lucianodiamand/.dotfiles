{ pkgs, ... }:

let
  pass-otp = pkgs.pass.withExtensions (e: [ e.pass-otp ]);
in {
  home.packages = [ pass-otp ];
}
