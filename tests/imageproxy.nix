{ common, ... }:

{
  name = "shabitica-imageproxy";

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
          ${mkAddrRRs "shabitica.example.org" nodes.shabitica}
          ${mkAddrRRs "unrelated.org" nodes.unrelated}
          ${mkAddrRRs "sslhost.org" nodes.sslhost}
        '';
      };
    };

    shabitica = {
      imports = [ common (mkNetwork 2) useResolver snakeOilCerts ];
      shabitica.hostName = "shabitica.example.org";
      shabitica.useSSL = false;
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
      services.nginx.virtualHosts."sslhost.org" = {
        onlySSL = true;
        enableACME = false;
        sslCertificate = ssl/snakeoil.cert;
        sslCertificateKey = ssl/snakeoil.key;

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
    # fmt: off
    import hashlib, hmac
    from base64 import urlsafe_b64encode, b64encode

    start_all()
    resolver.wait_for_unit('bind.service')
    unrelated.wait_for_open_port(80)
    client.wait_for_unit('multi-user.target')
    shabitica.wait_for_unit('shabitica.service')

    base_url = 'http://shabitica.example.org'
    valid_url = 'http://unrelated.org/test.png'
    valid_ssl_url = 'https://sslhost.org/test.png'

    with shabitica.nested('check whether DNS resolver works'):
      shabitica.succeed('host unrelated.org')

    with shabitica.nested('getting session secret'):
      secret = shabitica.succeed(
        'source /var/lib/shabitica/secrets.env && echo -n "$SESSION_SECRET"'
      ).encode()

    def mkrequest(url: str) -> str:
      encoded_url = urlsafe_b64encode(url.encode()).decode()
      proxy_url = f'{base_url}/imageproxy/{encoded_url.rstrip("=")}'
      sessraw = b'{"userId":"56756b7c-f0a8-4553-92c7-b1c553742828"}'
      sess = b64encode(sessraw).decode()
      hmac_msg = f'session={sess}'.encode()
      sigraw = hmac.new(secret, hmac_msg, hashlib.sha1).digest()
      sig = urlsafe_b64encode(sigraw).decode().rstrip('=')
      return f"-b 'session={sess}; session.sig={sig}' '{proxy_url}'"

    def curl(args: str) -> str:
      headers = "-H 'Pragma: no-cache' -H 'Cache-Control: no-cache'"
      return f"curl -f {headers} {args}"

    with subtest("images can be fetched directly"):
      client.succeed(curl(valid_url) + ' > direct.png')
      client.succeed('cmp direct.png ${green}')
      client.succeed(curl(valid_ssl_url) + ' > direct-ssl.png')
      client.succeed('cmp direct-ssl.png ${blue}')

    with subtest("refuses invalid session"):
      invalid_sess = 'dGhpcyBpcyBpbnZhbGlkCg=='
      invalid_sig = '0D1XbRNCLlS3Rk3-EP_zWjT_IA0'
      invalid_cookies = f'session={invalid_sess}; session.sig={invalid_sig}'
      encoded_url = urlsafe_b64encode(valid_url.encode()).decode()
      url = f'{base_url}/imageproxy/{encoded_url}'
      client.fail(curl(f"-b '{invalid_cookies}' '{url}'"))
      client.fail(curl(f"'{url}'"))

    with subtest("works with valid session"):
      client.execute(curl(mkrequest(valid_url)) + ' > /dev/null')
      client.execute(curl(mkrequest(valid_ssl_url)) + ' > /dev/null')

    with subtest("emits the right image"):
      client.succeed(curl(mkrequest(valid_url)) + ' > image.png')
      client.succeed('cmp image.png ${green}')
      client.succeed(curl(mkrequest(valid_ssl_url)) + ' > image-ssl.png')
      client.succeed('cmp image-ssl.png ${blue}')

    with subtest("caching works"):
      unrelated.shutdown()
      client.succeed(curl(mkrequest(valid_url)) + ' > cached.png')
      client.succeed('cmp cached.png ${green}')
      client.succeed(curl(mkrequest(valid_ssl_url)) + ' > cached-ssl.png')
      client.succeed('cmp cached-ssl.png ${blue}')
  '';
}
