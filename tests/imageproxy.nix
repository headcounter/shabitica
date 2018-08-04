{ common, ... }:

{
  name = "habitica-imageproxy";

  nodes = let
    mkNetwork = num: { lib, ... }: let
      v4 = "98.76.54.${toString num}";
      v6 = "abcd::${toString num}";
    in {
      networking.useDHCP = false;
      networking.interfaces.eth1 = {
        ipv4.addresses = lib.singleton { address = v4; prefixLength = 24; };
        ipv6.addresses = lib.singleton { address = v6; prefixLength = 64; };
      };
      networking.primaryIPAddress = "${v4} ${v6}";
    };
  in {
    habitica.imports = [ common (mkNetwork 1) ];
    client = mkNetwork 2;

    unrelated = { lib, pkgs, ... }: {
      imports = lib.singleton (mkNetwork 3);
      networking.firewall.enable = false;
      services.nginx.enable = true;
      services.nginx.virtualHosts.unrelated.root = pkgs.runCommand "docroot" {
        nativeBuildInputs = [ pkgs.imagemagick ];
      } ''
        mkdir -p "$out"
        convert -size 200x200 xc:green "$out/test.png"
      '';
    };
  };

  testScript = { nodes, ... }: let
    inherit (nodes.unrelated.config.services.nginx) virtualHosts;
    image = "${virtualHosts.unrelated.root}/test.png";
  in ''
    use Digest::SHA qw(hmac_sha1);
    use MIME::Base64 qw(encode_base64 encode_base64url);

    startAll;
    $unrelated->waitForOpenPort(80);
    $client->waitForUnit('multi-user.target');
    $habitica->waitForUnit('habitica.service');

    my $validUrl = 'http://unrelated/test.png';
    my $validProxyUrl = "http://habitica/imageproxy/$validUrl";
    my $secret;

    $habitica->nest('getting session secret', sub {
      $secret = $habitica->succeed(
        'source /var/lib/habitica/secrets.env && echo -n "$SESSION_SECRET"'
      );
    });

    sub mkRequest ($) {
      my $url = 'http://habitica/imageproxy/'.encode_base64url($_[0]);
      my $sessraw = '{"userId":"56756b7c-f0a8-4553-92c7-b1c553742828"}';
      my $sess = encode_base64($sessraw, "");
      my $sigraw = hmac_sha1('session='.$sess, $secret);
      my $sig = encode_base64url($sigraw);
      return "-b 'session=$sess; session.sig=$sig' '$url'";
    }

    subtest "image can be fetched directly", sub {
      $client->succeed('curl -f http://unrelated/test.png > direct.png');
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
      $client->succeed('curl -f '.mkRequest($validUrl).' > /dev/null');
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
