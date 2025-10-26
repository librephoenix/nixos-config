{ config, lib, ...}:

let
  cfg = config.systemSettings.brave;
in
{
  options = {
    systemSettings.brave = {
      enable = lib.mkEnableOption "Enable brave group policies";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.etc."/brave/policies/managed/GroupPolicy.json".text = ''
    {
     "BrowserSignin": 0,
      "PasswordManagerEnabled": false,
      "SpellcheckEnabled": true,
      "SpellcheckLanguage": [
        "en-US"
      ],
      "TorDisabled": true,
      "BraveRewardsDisabled": true,
      "BraveWalletDisabled": true,
      "BraveVPNDisabled": true,
      "BraveAIChatEnabled": false,
      "BraveNewsDisabled": true,
      "BraveTalkDisabled": true,
      "BraveSpeedreaderEnabled": false,
      "BraveP3AEnabled": false,
      "BraveStatsPingEnabled": false,
      "BraveWebDiscoveryEnabled": false,
      "BraveSyncUrl": "https://sync-v2.brave.com/v2",
      "DefaultSearchProviderAlternateURLS": [
        "https://search.nixos.org/packages?channel=unstable&query={searchTerms}"
      ]
    }
    '';
  };
}
