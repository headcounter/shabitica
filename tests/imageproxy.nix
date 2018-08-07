{ common, ... }:

{
  name = "habitica-imageproxy";

  nodes = let
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
        '';
      };
    };

    habitica = {
      imports = [ common (mkNetwork 2) useResolver ];
      habitica.hostName = "habitica.example.org";
      habitica.useSSL = false;
    };

    client.imports = [ (mkNetwork 3) useResolver ];

    unrelated = { lib, pkgs, ... }: {
      imports = [ (mkNetwork 4) useResolver ];
      networking.firewall.enable = false;
      services.nginx.enable = true;
      services.nginx.virtualHosts."unrelated.org"= {
        root = pkgs.runCommand "docroot" {
          nativeBuildInputs = [ pkgs.imagemagick ];
        } ''
          mkdir -p "$out"
          convert -size 200x200 xc:green "$out/test.png"
        '';
      };
    };
  };

  testScript = { nodes, ... }: let
    inherit (nodes.unrelated.config.services.nginx) virtualHosts;
    image = "${virtualHosts."unrelated.org".root}/test.png";
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
    my $validProxyUrl = "$baseUrl/imageproxy/$validUrl";
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

    subtest "image can be fetched directly", sub {
      $client->succeed('curl -f '.$validUrl.' > direct.png');
      $client->succeed('cmp direct.png ${image}');
    };

    subtest "refuses invalid session", sub {
      my $invalidSess = 'dGhpcyBpcyBpbnZhbGlkCg==';
      my $invalidSig = '0D1XbRNCLlS3Rk3-EP_zWjT_IA0';
      my $invalidCookies = "session=$invalidSess; session.sig=$invalidSig";
      $client->fail("curl -f -b '$invalidCookies' '$validProxyUrl'");
      $client->fail("curl -f '$validProxyUrl'");
    };

    subtest "works with valid session", sub {
      $client->execute('curl -f '.mkRequest($validUrl).' > /dev/null');
    };

    subtest "emits the right image", sub {
      $client->succeed('curl -f '.mkRequest($validUrl).' > image.png');
      $client->succeed('cmp image.png ${image}');
    };

    subtest "caching works", sub {
      $unrelated->shutdown;
      $client->succeed('curl -f '.mkRequest($validUrl).' > cached.png');
      $client->succeed('cmp cached.png ${image}');
    };
  '';
}
