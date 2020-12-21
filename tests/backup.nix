{ pkgs, lib, common, registerUser, ... }:

{
  name = "shabitica-backup";

  machine = {
    imports = [ common ];
    shabitica.hostName = "localhost";
  };

  testScript = let
    inherit (import ../pkgs/shabitica/docinfo.nix) dbrestore;

    mkPythonStr = val: "'${lib.escape ["\\" "'"] val}'";

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
      # This is so we can use Python variables in path.
      pathArg = "f\" http://localhost/api/v3/${path}\"";
    in "machine.succeed(${mkPythonStr cmd} + curl_auth_args + ${pathArg})";

  in ''
    # fmt: off
    from subprocess import check_output

    def pipe_read_line(data: str, cmd: str) -> str:
      result = check_output(cmd, shell=True, input=data.encode())
      return result.rstrip().decode()

    machine.wait_for_unit('shabitica.service')

    with machine.nested("add first user and get API token"):
      data = machine.succeed(${registerUser "foo" "localhost"})
      curl_auth_args = pipe_read_line(data, ${mkPythonStr getCurlAuthArgs})

    with machine.nested("create a new todo"):
      taskdata = ${callApi "tasks/user" "POST" {
        text = "first task";
        type = "todo";
      }}
      task_id = pipe_read_line(taskdata, ${mkPythonStr (mkJQ ".data.id")})

    with machine.nested("trigger backup"):
      machine.start_job('shabitica-db-backup.service')

    with machine.nested("change todo text"):
      ${callApi "tasks/{task_id}" "PUT" {
        text = "changed task";
      }}

    with machine.nested("verify changed todo text"):
      taskdata = ${callApi "tasks/{task_id}" "GET" {}}
      tasktext = pipe_read_line(taskdata, ${mkPythonStr (mkJQ ".data.text")})
      assert tasktext == 'changed task', f"invalid task text {tasktext!r}"

    with machine.nested("restore database"):
      machine.succeed('${dbrestore}"/var/backup/shabitica/'
                      '$(ls -1 /var/backup/shabitica | tail -n 1)"')

    with machine.nested("check whether task has old text"):
      taskdata = ${callApi "tasks/{task_id}" "GET" {}}
      tasktext = pipe_read_line(taskdata, ${mkPythonStr (mkJQ ".data.text")})
      assert tasktext == 'first task', f"invalid task text {tasktext!r}"
  '';
}
