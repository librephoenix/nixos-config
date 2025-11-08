{ pkgs, ... }:

{
  config = {
    systemSettings = {
      # users
      users = [ "emmet" ];
      adminUsers = [ "emmet" ];

      # hardware
      cachy.enable = true;
      cachy.variant = "lts";

      virtualization = {
        docker.enable = true;
      };

      # dotfiles
      dotfilesDir = "/etc/nixos";
      systemBuilder.enable = false;

      # security
      security = {
        automount.enable = false;
        blocklist.enable = true;
        doas.enable = true;
        firejail.enable = false; # TODO setup firejail profiles
        firewall.enable = true;
        gpg.enable = true;
        sshd.enable = true;
      };

      stylix = {
        enable = true;
        theme = "tomorrow-night";
      };

    };

    users.users.emmet.openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDaeejVJwUVrIZSo1isbu+gkQ7+8ftCgCsczy3OclkEVWHyRTqlG6yp74hr3j8ZNsOhov7c2Q6RqC8oy669hlxi/y9BsvtlI7sBr94oAKFOmkCS4RiK72ngJjBvI0vbk89wQQjmAd3r8B7ZcedpNOC8CkHu8SebKdYPRIUvAbPc3fTEt7DsJkazAepZCB8LEhUp57FAqQ/Ezlt3X/1uwNq5S0EbE9Zm+nUpEfSqR9apY2neKWLyGiCxpK3dzyNOuulCxvtVz+ie2sTk/6SxM+qWEoVVxhdwyxPihEjgC0EvtG0S5mVh5JmcjRkJOzzBHJuw+6r8yWn/AxGdIsoJ4rKNxH1XH1iLHgCraOLOUjUNlmejTcQPu6o92a79fdz2gCHT/BuIjfCW7MErAC3YSmF45TSur/kiWCBaTqYo06pgbQ3w1vKg7fievQlQzsutmg47RvJp6fb74yxuOdVg39cShQu/l8r6zqm21JAeUaaIp4P/0MrAIMOOVUhbK0QgsNElO4yn0ZKH8wGIF8xORh7ikxUIAyq8C41gjJiO2sAFJc3M8DhduQU3X0lHB7U0Qyu+8ZXn05+zdFPXJ73LKc7DCcLkppRXJsdHLSDEFdWqFnV7o08B4qZkPMT4pmvhwhY0Pf1fwavOqxuTstzw18gUGyQzl0foQi0Qrmdazsp2Qw== emmet@snowfire"
    ];

    environment.systemPackages = with pkgs; [
      rclone
      rdiff-backup
      rsnapshot
      cryptsetup
      gocryptfs
      attic-client
    ];

    programs.fuse.userAllowOther = true;

    networking.firewall.extraCommands =
      # ip ban ai crawlers
      let
        createDropRulesForIpAddress = address: ''
          iptables -A INPUT -s ${address} -j DROP
          iptables -A OUTPUT -s ${address} -j DROP
          iptables -A FORWARD -s ${address} -j DROP
          iptables -A DOCKER -s ${address} -j DROP
          iptables -A DOCKER-BRIDGE -s ${address} -j DROP
          iptables -A DOCKER-FORWARD -s ${address} -j DROP
          iptables -A DOCKER-USER -s ${address} -j DROP
          iptables -A DOCKER-ISOLATION-STAGE-1 -s ${address} -j DROP
          iptables -A DOCKER-ISOLATION-STAGE-2 -s ${address} -j DROP
        '';
      in
      ''
        ${createDropRulesForIpAddress "216.73.216.143"}
        ${createDropRulesForIpAddress "100.24.149.244"}
        ${createDropRulesForIpAddress "100.24.167.60"}
        ${createDropRulesForIpAddress "100.25.120.246"}
        ${createDropRulesForIpAddress "100.27.153.9"}
        ${createDropRulesForIpAddress "100.28.204.82"}
        ${createDropRulesForIpAddress "100.28.44.58"}
        ${createDropRulesForIpAddress "18.204.152.114"}
        ${createDropRulesForIpAddress "18.205.127.11"}
        ${createDropRulesForIpAddress "18.205.213.231"}
        ${createDropRulesForIpAddress "18.205.91.101"}
        ${createDropRulesForIpAddress "18.209.201.119"}
        ${createDropRulesForIpAddress "18.210.58.238"}
        ${createDropRulesForIpAddress "18.211.148.239"}
        ${createDropRulesForIpAddress "18.213.102.186"}
        ${createDropRulesForIpAddress "18.214.138.148"}
        ${createDropRulesForIpAddress "18.215.112.101"}
        ${createDropRulesForIpAddress "18.233.24.238"}
        ${createDropRulesForIpAddress "184.72.95.195"}
        ${createDropRulesForIpAddress "184.73.167.217"}
        ${createDropRulesForIpAddress "184.73.239.35"}
        ${createDropRulesForIpAddress "23.20.178.124"}
        ${createDropRulesForIpAddress "23.21.119.232"}
        ${createDropRulesForIpAddress "23.21.175.228"}
        ${createDropRulesForIpAddress "23.21.227.240"}
        ${createDropRulesForIpAddress "23.22.105.143"}
        ${createDropRulesForIpAddress "23.22.59.87"}
        ${createDropRulesForIpAddress "23.23.137.202"}
        ${createDropRulesForIpAddress "23.23.180.225"}
        ${createDropRulesForIpAddress "23.23.212.212"}
        ${createDropRulesForIpAddress "23.23.213.182"}
        ${createDropRulesForIpAddress "3.208.146.193"}
        ${createDropRulesForIpAddress "3.210.114.189"}
        ${createDropRulesForIpAddress "3.210.223.61"}
        ${createDropRulesForIpAddress "3.210.29.96"}
        ${createDropRulesForIpAddress "3.211.105.134"}
        ${createDropRulesForIpAddress "3.211.181.86"}
        ${createDropRulesForIpAddress "3.212.205.90"}
        ${createDropRulesForIpAddress "3.213.85.234"}
        ${createDropRulesForIpAddress "3.215.221.125"}
        ${createDropRulesForIpAddress "3.216.13.10"}
        ${createDropRulesForIpAddress "3.216.86.144"}
        ${createDropRulesForIpAddress "3.217.171.106"}
        ${createDropRulesForIpAddress "3.218.103.254"}
        ${createDropRulesForIpAddress "3.219.81.66"}
        ${createDropRulesForIpAddress "3.221.222.168"}
        ${createDropRulesForIpAddress "3.223.134.5"}
        ${createDropRulesForIpAddress "3.225.9.97"}
        ${createDropRulesForIpAddress "3.227.180.70"}
        ${createDropRulesForIpAddress "3.232.82.72"}
        ${createDropRulesForIpAddress "3.235.215.92"}
        ${createDropRulesForIpAddress "34.193.2.57"}
        ${createDropRulesForIpAddress "34.194.14.255"}
        ${createDropRulesForIpAddress "34.194.233.48"}
        ${createDropRulesForIpAddress "34.195.248.30"}
        ${createDropRulesForIpAddress "34.197.28.78"}
        ${createDropRulesForIpAddress "34.203.111.15"}
        ${createDropRulesForIpAddress "34.205.170.13"}
        ${createDropRulesForIpAddress "34.206.249.188"}
        ${createDropRulesForIpAddress "34.224.132.215"}
        ${createDropRulesForIpAddress "34.225.87.80"}
        ${createDropRulesForIpAddress "34.226.89.140"}
        ${createDropRulesForIpAddress "34.231.156.59"}
        ${createDropRulesForIpAddress "34.233.114.237"}
        ${createDropRulesForIpAddress "34.234.197.175"}
        ${createDropRulesForIpAddress "34.234.200.207"}
        ${createDropRulesForIpAddress "35.168.238.50"}
        ${createDropRulesForIpAddress "35.169.119.108"}
        ${createDropRulesForIpAddress "35.169.240.53"}
        ${createDropRulesForIpAddress "35.170.205.140"}
        ${createDropRulesForIpAddress "35.173.38.202"}
        ${createDropRulesForIpAddress "3.93.211.16"}
        ${createDropRulesForIpAddress "3.94.199.128"}
        ${createDropRulesForIpAddress "44.205.120.22"}
        ${createDropRulesForIpAddress "44.205.74.196"}
        ${createDropRulesForIpAddress "44.206.65.8"}
        ${createDropRulesForIpAddress "44.207.207.36"}
        ${createDropRulesForIpAddress "44.207.252.58"}
        ${createDropRulesForIpAddress "44.209.35.147"}
        ${createDropRulesForIpAddress "44.214.19.8"}
        ${createDropRulesForIpAddress "44.215.235.20"}
        ${createDropRulesForIpAddress "44.218.170.184"}
        ${createDropRulesForIpAddress "44.220.2.97"}
        ${createDropRulesForIpAddress "44.221.180.179"}
        ${createDropRulesForIpAddress "44.221.227.90"}
        ${createDropRulesForIpAddress "44.223.115.10"}
        ${createDropRulesForIpAddress "44.223.116.149"}
        ${createDropRulesForIpAddress "44.223.232.55"}
        ${createDropRulesForIpAddress "50.19.102.70"}
        ${createDropRulesForIpAddress "50.19.79.213"}
        ${createDropRulesForIpAddress "52.0.218.219"}
        ${createDropRulesForIpAddress "52.0.63.151"}
        ${createDropRulesForIpAddress "52.200.142.199"}
        ${createDropRulesForIpAddress "52.202.233.37"}
        ${createDropRulesForIpAddress "52.203.152.231"}
        ${createDropRulesForIpAddress "52.203.65.83"}
        ${createDropRulesForIpAddress "52.204.174.139"}
        ${createDropRulesForIpAddress "52.204.71.8"}
        ${createDropRulesForIpAddress "52.204.89.12"}
        ${createDropRulesForIpAddress "52.205.113.104"}
        ${createDropRulesForIpAddress "52.21.62.139"}
        ${createDropRulesForIpAddress "52.2.191.202"}
        ${createDropRulesForIpAddress "52.22.87.224"}
        ${createDropRulesForIpAddress "52.3.102.51"}
        ${createDropRulesForIpAddress "52.3.127.170"}
        ${createDropRulesForIpAddress "52.3.155.146"}
        ${createDropRulesForIpAddress "52.4.213.199"}
        ${createDropRulesForIpAddress "52.4.229.9"}
        ${createDropRulesForIpAddress "52.4.238.8"}
        ${createDropRulesForIpAddress "52.45.15.233"}
        ${createDropRulesForIpAddress "52.45.92.83"}
        ${createDropRulesForIpAddress "52.54.249.218"}
        ${createDropRulesForIpAddress "52.54.95.127"}
        ${createDropRulesForIpAddress "52.6.5.24"}
        ${createDropRulesForIpAddress "52.70.123.241"}
        ${createDropRulesForIpAddress "52.71.216.196"}
        ${createDropRulesForIpAddress "52.71.218.25"}
        ${createDropRulesForIpAddress "52.73.6.26"}
        ${createDropRulesForIpAddress "54.145.82.217"}
        ${createDropRulesForIpAddress "54.147.238.89"}
        ${createDropRulesForIpAddress "54.147.80.137"}
        ${createDropRulesForIpAddress "54.156.55.147"}
        ${createDropRulesForIpAddress "54.157.84.74"}
        ${createDropRulesForIpAddress "54.159.18.27"}
        ${createDropRulesForIpAddress "54.159.98.248"}
        ${createDropRulesForIpAddress "54.162.69.192"}
        ${createDropRulesForIpAddress "54.163.136.244"}
        ${createDropRulesForIpAddress "54.167.32.123"}
        ${createDropRulesForIpAddress "54.197.114.76"}
        ${createDropRulesForIpAddress "54.225.181.161"}
        ${createDropRulesForIpAddress "54.225.199.17"}
        ${createDropRulesForIpAddress "54.235.125.129"}
        ${createDropRulesForIpAddress "54.243.63.52"}
        ${createDropRulesForIpAddress "54.83.180.239"}
        ${createDropRulesForIpAddress "54.83.56.1"}
        ${createDropRulesForIpAddress "54.85.7.119"}
        ${createDropRulesForIpAddress "54.88.84.219"}
        ${createDropRulesForIpAddress "54.89.90.224"}
        ${createDropRulesForIpAddress "98.82.39.241"}
        ${createDropRulesForIpAddress "98.83.10.183"}
        ${createDropRulesForIpAddress "98.83.177.42"}
        ${createDropRulesForIpAddress "98.83.8.142"}
        ${createDropRulesForIpAddress "98.84.131.195"}
        ${createDropRulesForIpAddress "98.84.184.80"}
        ${createDropRulesForIpAddress "98.84.200.43"}
        ${createDropRulesForIpAddress "98.84.60.17"}
        ${createDropRulesForIpAddress "98.84.70.201"}
      '';
    virtualisation.docker.extraOptions = "--iptables=true";
  };

}
