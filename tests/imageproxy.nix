{ common, ... }:

{
  name = "habitica-imageproxy";

  nodes = let
    snakeOilCerts = { lib, nodes, ... }: let
      inherit (nodes.sslhost.config.services.nginx) virtualHosts;
      inherit (virtualHosts."sslhost.org") sslCertificate;
    in { security.pki.certificateFiles = lib.singleton sslCertificate; };

    mkNetwork = num: { lib, ... }: let
      v4 = "98.76.54.${toString num}";
      v6 = "abcd::${toString num}";
      forceAddr = attrs: lib.mkForce (lib.singleton attrs);
    in {
      networking.useDHCP = false;
      networking.interfaces.eth1 = {
        ipv4.addresses = forceAddr { address = v4; prefixLength = 24; };
        ipv6.addresses = forceAddr { address = v6; prefixLength = 64; };
      };
    };

    useResolver = { lib, nodes, ... }: {
      networking.nameservers = let
        inherit (nodes.resolver.config.networking.interfaces.eth1) ipv4 ipv6;
        getAddrs = attr: map (a: a.address) attr.addresses;
      in lib.mkForce (getAddrs ipv4 ++ getAddrs ipv6);
    };

  in {
    resolver = { config, pkgs, nodes, lib, ... }: {
      imports = lib.singleton (mkNetwork 1);
      networking.firewall.enable = false;
      services.bind.enable = true;
      services.bind.cacheNetworks = lib.mkForce [ "any" ];
      services.bind.forwarders = lib.mkForce [];
      services.bind.zones = lib.singleton {
        name = ".";
        file = let
          mkAddrRRs = fqdn: node: let
            inherit (node.config.networking.interfaces.eth1) ipv4 ipv6;
            mkRR = rtype: addr: "${fqdn}. IN ${rtype} ${addr.address}\n";
            mkRRs = rtype: lib.concatMapStrings (mkRR rtype);
          in mkRRs "A" ipv4.addresses + mkRRs "AAAA" ipv6.addresses;
        in pkgs.writeText "fake-root.zone" ''
          $TTL 3600
          . IN SOA ns.fakedns. admin.fakedns. ( 1 3h 1h 1w 1d )
          . IN NS ns.fakedns.
          ${mkAddrRRs "ns.fakedns" nodes.resolver}
          ${mkAddrRRs "habitica.example.org" nodes.habitica}
          ${mkAddrRRs "unrelated.org" nodes.unrelated}
          ${mkAddrRRs "sslhost.org" nodes.sslhost}
        '';
      };
    };

    habitica = {
      imports = [ common (mkNetwork 2) useResolver snakeOilCerts ];
      habitica.hostName = "habitica.example.org";
      habitica.useSSL = false;
    };

    client.imports = [ (mkNetwork 3) useResolver snakeOilCerts ];

    unrelated = { lib, pkgs, ... }: {
      imports = [ (mkNetwork 4) useResolver ];
      networking.firewall.enable = false;
      services.nginx.enable = true;
      services.nginx.virtualHosts."unrelated.org" = {
        root = pkgs.runCommand "docroot" {
          nativeBuildInputs = [ pkgs.imagemagick ];
        } ''
          mkdir -p "$out"
          convert -size 200x200 xc:green "$out/test.png"
        '';
      };
    };

    sslhost = { lib, pkgs, ... }: {
      imports = [ (mkNetwork 5) useResolver ];
      networking.firewall.enable = false;
      services.nginx.enable = true;
      services.nginx.virtualHosts."sslhost.org" = let
        keypair = pkgs.runCommand "snakeoil-keys" {
          nativeBuildInputs = [ pkgs.openssl ];
        } ''
          mkdir "$out"
          openssl req -nodes -x509 -newkey rsa:2048 -days 65535 \
            -subj '/CN=sslhost.org' \
            -keyout "$out/key.pem" -out "$out/cert.pem"
        '';
      in {
        onlySSL = true;
        enableACME = false;
        sslCertificate = "${keypair}/cert.pem";
        sslCertificateKey = "${keypair}/key.pem";

        root = pkgs.runCommand "docroot-ssl" {
          nativeBuildInputs = [ pkgs.imagemagick ];
        } ''
          mkdir -p "$out"
          convert -size 200x200 xc:blue "$out/test.png"
        '';
      };
    };
  };

  testScript = { nodes, ... }: let
    green = let
      inherit (nodes.unrelated.config.services.nginx) virtualHosts;
    in "${virtualHosts."unrelated.org".root}/test.png";
    blue = let
      inherit (nodes.sslhost.config.services.nginx) virtualHosts;
    in "${virtualHosts."sslhost.org".root}/test.png";
  in ''
    use Digest::SHA qw(hmac_sha1);
    use MIME::Base64 qw(encode_base64 encode_base64url);

    startAll;
    $resolver->waitForUnit('bind.service');
    $unrelated->waitForOpenPort(80);
    $client->waitForUnit('multi-user.target');
    $habitica->waitForUnit('habitica.service');

    my $baseUrl = 'http://habitica.example.org';
    my $validUrl = 'http://unrelated.org/test.png';
    my $validSslUrl = 'https://sslhost.org/test.png';
    my $secret;

    $habitica->nest('check whether DNS resolver works', sub {
      $habitica->succeed('host unrelated.org');
    });

    $habitica->nest('getting session secret', sub {
      $secret = $habitica->succeed(
        'source /var/lib/habitica/secrets.env && echo -n "$SESSION_SECRET"'
      );
    });

    sub mkRequest ($) {
      my $url = $baseUrl.'/imageproxy/'.encode_base64url($_[0]);
      my $sessraw = '{"userId":"56756b7c-f0a8-4553-92c7-b1c553742828"}';
      my $sess = encode_base64($sessraw, "");
      my $sigraw = hmac_sha1('session='.$sess, $secret);
      my $sig = encode_base64url($sigraw);
      return "-b 'session=$sess; session.sig=$sig' '$url'";
    }

    sub curl ($) {
      my $headers = "-H 'Pragma: no-cache' -H 'Cache-Control: no-cache'";
      return "curl -f $headers $_[0]";
    }

    subtest "images can be fetched directly", sub {
      $client->succeed(curl($validUrl).' > direct.png');
      $client->succeed('cmp direct.png ${green}');
      $client->succeed(curl($validSslUrl).' > direct-ssl.png');
      $client->succeed('cmp direct-ssl.png ${blue}');
    };

    subtest "refuses invalid session", sub {
      my $invalidSess = 'dGhpcyBpcyBpbnZhbGlkCg==';
      my $invalidSig = '0D1XbRNCLlS3Rk3-EP_zWjT_IA0';
      my $invalidCookies = "session=$invalidSess; session.sig=$invalidSig";
      my $url = $baseUrl.'/imageproxy/'.encode_base64url($validUrl);
      $client->fail(curl("-b '$invalidCookies' '$url'"));
      $client->fail(curl("'$url'"));
    };

    subtest "works with valid session", sub {
      $client->execute(curl(mkRequest($validUrl)).' > /dev/null');
      $client->execute(curl(mkRequest($validSslUrl)).' > /dev/null');
    };

    subtest "emits the right image", sub {
      $client->succeed(curl(mkRequest($validUrl)).' > image.png');
      $client->succeed('cmp image.png ${green}');
      $client->succeed(curl(mkRequest($validSslUrl)).' > image-ssl.png');
      $client->succeed('cmp image-ssl.png ${blue}');
    };

    subtest "caching works", sub {
      $unrelated->shutdown;
      $client->succeed(curl(mkRequest($validUrl)).' > cached.png');
      $client->succeed('cmp cached.png ${green}');
      $client->succeed(curl(mkRequest($validSslUrl)).' > cached-ssl.png');
      $client->succeed('cmp cached-ssl.png ${blue}');
    };
  '';
}
