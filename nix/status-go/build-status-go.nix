{ buildGoPackage, go, xcodeWrapper, stdenv }:

{ owner, repo, rev, version, goPackagePath, src, host,
  nativeBuildInputs ? [],
  buildPhase,
  installPhase ? "",
  postInstall ? "",
  outputs, meta } @ args:

with stdenv;

let
  removeReferences = [ go ];
  removeExpr = refs: ''remove-references-to ${lib.concatMapStrings (ref: " -t ${ref}") refs}'';

  buildStatusGo = buildGoPackage (args // {
    name = "${repo}-${version}-${host}";

    nativeBuildInputs = nativeBuildInputs ++ lib.optional isDarwin xcodeWrapper;

    # Fixes Cgo related build failures (see https://github.com/NixOS/nixpkgs/issues/25959 )
    hardeningDisable = [ "fortify" ];

    # gomobile doesn't seem to be able to pass -ldflags with multiple values correctly to go build, so we just patch files here  
    patchPhase = ''
      date=$(date -u '+%Y-%m-%d.%H:%M:%S')

      substituteInPlace cmd/statusd/main.go --replace \
        "buildStamp = \"N/A\"" \
        "buildStamp = \"$date\""
      substituteInPlace params/version.go --replace \
        "var Version string" \
        "var Version string = \"${version}\""
      substituteInPlace params/version.go --replace \
        "var GitCommit string" \
        "var GitCommit string = \"${rev}\""
      substituteInPlace vendor/github.com/ethereum/go-ethereum/metrics/metrics.go --replace \
        "var EnabledStr = \"false\"" \
        "var EnabledStr = \"true\""
    '';

    # we print out the version so that we fail fast in case there's any problem running xcrun, instead of failing at the end of the build
    preConfigure = lib.optionalString isDarwin ''
      xcrun xcodebuild -version
    '';

    # remove hardcoded paths to go package in /nix/store, otherwise Nix will fail the build
    preFixup = ''
      find $out -type f -exec ${removeExpr removeReferences} '{}' + || true
      return
    '';

    meta = {
      # Add default meta information
      inherit (meta) platforms;
      description = meta.description or "The Status module that consumes go-ethereum.";
      license = lib.licenses.mpl20;
    } // meta // {
      # add an extra maintainer to every package
      maintainers = (meta.maintainers or []) ++
                    [ lib.maintainers.pombeirp ];
    };
  });

in buildStatusGo