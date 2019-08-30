# This file is specifically to keep commands in documentation in sync with its
# implementation and tests. The idea is that we have this one place where we
# define specifics such as date formats and commands and use them in tests or
# assertions so that an eval or build will fail if something is wrong.
rec {
  dbrestore = "shabitica-db-restore --drop --archive=";

  # XXX: No assertion yet, so make sure to keep both in par!
  archiveDateFormat = "%Y-%m-%dT%H:%M:%S";
  archiveExampleFilename = "2018-02-12T12:22:09.archive";

  migrationMsg = "Running migration";
}
