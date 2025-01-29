{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {
  imports = [
    inputs.nur.modules.homeManager.default
  ];

  config = lib.mkMerge [
    {
      allowedUnfree = [
        "languagetool"
        "onepassword-password-manager"
        "tampermonkey"
      ];

      # home.packages = with pkgs; [
      #   tridactyl-native
      # ];

      programs.firefox = {
        enable = true;
        package = let
          darwin-firefox = pkgs.stdenv.mkDerivation rec {
            pname = "firefox";
            version = pkgs.firefox.version;

            buildInputs = [pkgs.undmg];
            sourceRoot = ".";
            phases = ["unpackPhase" "installPhase"];
            installPhase = ''
              mkdir -p "$out/Applications"
              cp -r Firefox.app "$out/Applications/Firefox.app"
            '';

            src = pkgs.fetchurl {
              name = "Firefox-${version}.dmg";
              url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/en-GB/Firefox%20${version}.dmg";
              sha256 = "sha256-xUIYUogS/QqLRaWZkNvGRfoePmzJ/seUnXfeV/eIbvI=";
            };

            meta = {
              description = "The Firefox web browser";
              homepage = "https://www.mozilla.org/en-GB/firefox";
              platforms = lib.platforms.darwin;
            };
          };
        in
          if builtins.elem pkgs.system lib.platforms.darwin
          then darwin-firefox
          else pkgs.firefox;
        enableGnomeExtensions = false;
        policies = {
          DefaultDownloadDirectory = "${config.home.homeDirectory}/downloads";
          DisableFirefoxStudies = true;
          DisablePocket = true;
          DisableTelemetry = true;
          DisableFirefoxAccounts = false;
          NoDefaultBookmarks = true;
          OfferToSaveLogins = false;
          OfferToSaveLoginsDefault = false;
          PasswordManagerEnabled = false;
          FirefoxHome = {
            Search = true;
            Pocket = false;
            Snippets = false;
            TopSites = false;
            Highlights = false;
          };
        };
        profiles.default = {
          id = 0;
          isDefault = true;
          name = "default";
          extensions = with pkgs.nur.repos.rycee.firefox-addons; [
            # bitwarden
            canvasblocker
            clearurls
            decentraleyes
            disconnect
            duckduckgo-privacy-essentials
            facebook-container
            firefox-color
            languagetool
            onepassword-password-manager
            privacy-badger
            return-youtube-dislikes
            sidebery
            sponsorblock
            stylus
            # tab-reloader
            tampermonkey
            # tridactyl
            ublock-origin
            youtube-nonstop
          ];
          containers = {
            personal = {
              id = 3;
              name = "Personal";
              icon = "fingerprint";
              color = "purple";
            };
            parkland = {
              id = 2;
              name = "Parkland";
              icon = "tree";
              color = "green";
            };
            uiuc = {
              id = 1;
              name = "UIUC";
              icon = "tree";
              color = "orange";
            };
            facebook = {
              id = 0;
              name = "Facebook";
              icon = "fence";
              color = "blue";
            };
          };
          containersForce = true;
          search = {
            force = true;
            default = "DuckDuckGo";
            privateDefault = "DuckDuckGo";
            engines = {
              "Nix Packages" = {
                urls = [
                  {
                    template = "https://search.nixos.org/packages";
                    params = [
                      {
                        name = "channel";
                        value = "unstable";
                      }
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];
                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = ["@np"];
              };
              "Nix Options" = {
                urls = [
                  {
                    template = "https://search.nixos.org/options";
                    params = [
                      {
                        name = "channel";
                        value = "unstable";
                      }
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];
                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = ["@no"];
              };
              "Home Manager Options" = {
                urls = [
                  {
                    template = "https://home-manager-options.extranix.com/";
                    params = [
                      {
                        name = "query";
                        value = "{searchTerms}";
                      }
                    ];
                  }
                ];
                icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
                definedAliases = ["@hm"];
              };
              "Google".metaData.hidden = true;
              "Amazon.com".metaData.hidden = true;
              "Bing".metaData.hidden = true;
              "eBay".metaData.hidden = true;
            };
          };
          settings = {
            "browser.aboutConfig.showWarning" = false;
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
            "browser.gesture.swipe.left" = "";
            "browser.gesture.swipe.right" = "";
            # "layout.css.devPixelsPerPx" = 1.1;
          };
          userChrome =
            # css
            ''
              #TabsToolbar {
                visibility: collapse;
              }
            '';
        };
      };
    }
    (lib.mkIf (builtins.elem pkgs.system lib.platforms.linux) {
      xdg.desktopEntries = let
        firefox = "firefox";
      in {
        ${firefox} = {
          name = "Firefox";
          exec = "${firefox} -P default %U";
          icon = "${firefox}";
          startupNotify = true;
          terminal = false;
          genericName = "Web Browser";
          categories = ["Network" "WebBrowser"];
        };
        firefox-profile-manager = {
          name = "Firefox Profile Manager";
          exec = "${firefox} -ProfileManager";
          icon = "${firefox}";
          startupNotify = true;
          terminal = false;
          genericName = "Web Browser";
          categories = ["Network" "WebBrowser"];
        };
      };
    })
    (lib.mkIf (builtins.elem pkgs.system lib.platforms.darwin) {
      nixpkgs.overlays = [inputs.nixpkgs-firefox-darwin.overlay];
    })
  ];
}
