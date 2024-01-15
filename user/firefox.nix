{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    inputs.nur.hmModules.nur
  ];

  home.packages = with pkgs; [
    firefox-devedition
    tridactyl-native
  ];

  # TODO: figure out why enabling this causes firefox to throw a missing
  # profile error and fix it so firefox can be configured declaratively
  programs.firefox = {
    enable = false;
    package = pkgs.firefox-devedition;
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
    profiles = {
      me = {
        id = 0;
        # name = "me";
        extensions = with pkgs.nur.repos.rycee.firefox-addons; [
          clearurls
          decentraleyes
          disconnect
          duckduckgo-privacy-essentials
          facebook-container
          languagetool
          multi-account-containers
          onepassword-password-manager
          privacy-badger
          return-youtube-dislikes
          sponsorblock
          stylus
          tab-reloader
          tampermonkey
          tridactyl
          ublock-origin
          youtube-nonstop
        ];
        search = {
          force = true;
          default = "DuckDuckGo";
          privateDefault = "DuckDuckGo";
          engines = {
            "Nix Packages" = {
              urls = [{
                template = "https://search.nixos.org/packages";
                params = [
                  { name = "channel"; value = "unstable"; }
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }];
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@np" ];
            };
            "Nix Options" = {
              urls = [{
                template = "https://search.nixos.org/options";
                params = [
                  { name = "channel"; value = "unstable"; }
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }];
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@no" ];
            };
            "Home Manager Options" = {
              urls = [{
                template = "https://mipmip.github.io/home-manager-option-search/";
                params = [
                  { name = "query"; value = "{searchTerms}"; }
                ];
              }];
              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@hm" ];
            };
            "Google".metaData.hidden = true;
            "Amazon.com".metaData.hidden = true;
            "Bing".metaData.hidden = true;
            "eBay".metaData.hidden = true;
          };
        };
        settings = {
          "browser.aboutConfig.showWarning" = false;
        };
      };
    };
  };
}
