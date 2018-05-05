# This lists all of the upgrades required to transform the database to the
# latest version. Of course order is important, because the upgrades are
# applied in sequence.
[
  { file = "groups/migrate-chat.js";
    testScript = ''
      spec = getuser('foo')
      party = spec.api.groups[spec.partyId].get()
      assertEquals(party['chat'][0]['text'], "Hello World!")
    '';
  }
]
