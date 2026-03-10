{ pkgs, ... }:

let
  addons = pkgs.nur.repos.rycee.firefox-addons;
in {
  programs.firefox = {
    enable = true;

    profiles.privacy = {
      isDefault = true;
      extensions.packages = with addons; [
        ublock-origin
        privacy-badger
        clearurls
        localcdn
        cookie-autodelete
      ];
      settings = {
        "extensions.autoDisableScopes" = 0;
        "extensions.enabledScopes" = 15;
        "browser.contentblocking.category" = "strict";
        "network.cookie.cookieBehavior" = 1;
        "privacy.donottrackheader.enabled" = true;
        "privacy.globalprivacycontrol.enabled" = true;
        "privacy.resistFingerprinting" = true;
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.pbmode.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
        "network.dns.echconfig.enabled" = true;
        "network.dns.echconfig.fallback_to_origin_when_all_failed" = false;
        "network.trr.mode" = 2;
        "network.trr.uri" = "https://mozilla.cloudflare-dns.com/dns-query";
        "app.shield.optoutstudies.enabled" = false;
        "datareporting.healthreport.uploadEnabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.server" = "";
        "browser.ping-centre.telemetry" = false;
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "browser.newtabpage.activity-stream.feeds.snippets" = false;
        "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
        "browser.newtabpage.activity-stream.feeds.discoverystreamfeed" = false;
        "browser.newtabpage.activity-stream.showSponsored" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
        "browser.urlbar.suggest.quicksuggest.nonsponsored" = false;
        "browser.urlbar.suggest.quicksuggest.sponsored" = false;
        "browser.urlbar.suggest.searches" = false;
        "browser.urlbar.suggest.topsites" = false;
        "browser.urlbar.suggest.engines" = false;
        "browser.urlbar.suggest.bookmark" = true;
        "browser.urlbar.suggest.history" = true;
        "network.prefetch-next" = false;
        "network.http.speculative-parallel-limit" = 0;
        "browser.urlbar.speculativeConnect.enabled" = false;
        "browser.cache.prefetch" = false;
        "browser.safebrowsing.malware.enabled" = true;
        "browser.safebrowsing.phishing.enabled" = true;
        "dom.security.https_only_mode" = true;
        "dom.security.https_only_mode_ever_enabled" = true;
        "browser.translations.enable" = false;
        "dom.webnotifications.enabled" = false;
        "dom.push.enabled" = false;
        "permissions.default.desktop-notification" = 2;
      };
    };
  };
}
