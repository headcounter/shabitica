import <nixpkgs/nixos/tests/make-test.nix> {
  name = "habitica";

  machine = {
    imports = [ ./. ];
    networking.hosts."127.0.0.1" = [ "habitica.example.org" ];
    networking.firewall.enable = false;
    services.nginx.enable = true;
    services.nginx.virtualHosts."habitica.example.org" = {
      locations."/".proxyPass = "http://unix:/run/habitica.sock:";
    };
  };

  testScript = ''
    $machine->waitForUnit('nginx.service');
    $machine->waitForOpenPort(80);
    $machine->execute('curl http://habitica.example.org/ >&2');
    $machine->sleep(20);
    $machine->execute('curl http://habitica.example.org/ >&2');
    $machine->execute('ls -la /run >&2');
    $machine->execute('ls -la /run/habitica >&2');
    $machine->execute('netstat -ntlpue >&2');
  '';
}
