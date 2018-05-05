{ pkgs, lib, common, registerUser, ... }:

{
  name = "habitica-backup";

  machine = {
    imports = [ common ];
    habitica.hostName = "localhost";
    habitica.backupInterval = "daily";
  };

  testScript = let
    inherit (import ../docinfo.nix) dbrestore;

    mkPerlStr = val: "'${lib.escape ["\\" "'"] val}'";

    mkJQ = expr: "${pkgs.jq}/bin/jq -r ${lib.escapeShellArg expr}";

    getCurlAuthArgs = let
      jqExpr = "@sh \"-H \\(\"x-api-user: \" + .data.id) "
             + "-H \\(\"x-api-key: \" + .data.apiToken)\"";
    in mkJQ jqExpr;

    callApi = path: method: data: let
      fullPath = lib.escapeShellArg "http://localhost/api/v3/${path}";
      shellData = lib.escapeShellArg (builtins.toJSON data);
      extra = lib.optionalString (data != {}) " --data-raw ${shellData}";
      contentType = "-H 'Content-Type: application/json'";
      cmd = "curl -f ${contentType} -X ${method}${extra} ";
      # This is so we can use Perl variables in path.
      pathArg = "\" http://localhost/api/v3/${path}\"";
    in "\$machine->succeed(${mkPerlStr cmd}.$curlAuthArgs.${pathArg})";

  in ''
    use IPC::Open2;

    sub pipeReadLine {
      my ($data, $cmd) = @_;
      my ($dataOut, $dataIn);
      open2 $dataOut, $dataIn, $cmd;
      print $dataIn $data;
      close $dataIn;
      my $result = <$dataOut>;
      close $dataOut;
      chomp $result;
      return $result;
    }

    $machine->waitForUnit('habitica.service');

    $machine->succeed('systemctl list-timers | grep -q habitica-db-backup');

    my $curlAuthArgs;

    $machine->nest("add first user and get API token", sub {
      my $data = $machine->succeed(${registerUser "foo" "localhost"});
      $curlAuthArgs = pipeReadLine $data, ${mkPerlStr getCurlAuthArgs};
    });

    my $taskId;

    $machine->nest("create a new todo", sub {
      my $taskdata = ${callApi "tasks/user" "POST" {
        text = "first task";
        type = "todo";
      }};
      $taskId = pipeReadLine $taskdata, ${mkPerlStr (mkJQ ".data.id")};
    });

    # The unit info before we're going to trigger it by forwarding time.
    my $oldInfo = $machine->getUnitInfo('habitica-db-backup.service');

    $machine->nest("trigger backup by fast-forwarding three days", sub {
      $machine->succeed('date "$(date +%m%d%H%M%Y.%S -d tomorrow)"');
    });

    $machine->nest("wait until backup is done", sub {
      my $oldStarted = $oldInfo->{ExecMainStartTimestampMonotonic};
      Machine::retry sub {
        my $info = $machine->getUnitInfo('habitica-db-backup.service');
        return 0 unless $info->{ExecMainStartTimestampMonotonic} > $oldStarted;
        return 1 if $info->{ActiveState} eq "inactive";
        return 0;
      };
    });

    $machine->nest("change todo text", sub {
      ${callApi "tasks/$taskId" "PUT" {
        text = "changed task";
      }};
    });

    $machine->nest("verify changed todo text", sub {
      my $taskdata = ${callApi "tasks/$taskId" "GET" {}};
      my $taskText = pipeReadLine $taskdata, ${mkPerlStr (mkJQ ".data.text")};
      die "invalid task text $taskText" unless $taskText eq "changed task";
    });

    $machine->nest("restore database", sub {
      $machine->succeed('${dbrestore}"/var/backup/habitica/'
                       .'$(ls -1 /var/backup/habitica | tail -n 1)"');
    });

    $machine->nest("check whether task has old text", sub {
      my $taskdata = ${callApi "tasks/$taskId" "GET" {}};
      my $taskText = pipeReadLine $taskdata, ${mkPerlStr (mkJQ ".data.text")};
      die "invalid task text $taskText" unless $taskText eq "first task";
    });
  '';
}
