# This lists all of the upgrades required to transform the database to the
# latest version. Of course order is important, because the upgrades are
# applied in sequence.
[
  { file = "groups/migrate-chat.js";
    testScript = ''
      spec = getuser('foo')
      party = spec.api.groups[spec.partyId].get()
      assertEqual(party['chat'][0]['text'], "Hello World!")
    '';
  }
  { file = "tasks/habits-one-history-entry-per-day-users.js";
    testScript = ''
      spec = getuser('foo')
      habit = spec.api.tasks[spec.habitId].get()

      assertIn('history', habit)
      assertEqual(len(habit['history']), 2)

      for hist in habit['history']:
        assertIn('date', hist)
        assertIn('value', hist)
        assertIn('scoredUp', hist)
        assertIn('scoredDown', hist)

      assertEqual(habit['history'][0]['scoredUp'], 3)
      assertEqual(habit['history'][0]['scoredDown'], 1)

      assertEqual(habit['history'][1]['scoredUp'], 1)
      assertEqual(habit['history'][1]['scoredDown'], 0)
    '';
  }
  { file = "tasks/habits-one-history-entry-per-day-challenges.js";
    testScript = ''
      spec = getuser('foo')
      challenge_tasks = spec.api.tasks.challenge[spec.challengeId].get()

      assertEqual(len(challenge_tasks), 1)

      challenge_habit = challenge_tasks[0]
      print(challenge_habit['history'])

      for hist in challenge_habit['history']:
        assertIn('date', hist)
        assertIn('value', hist)
        assertIn('scoredUp', hist)
        assertIn('scoredDown', hist)

      assertEqual(challenge_habit['history'][0]['scoredUp'], 0)
      assertEqual(challenge_habit['history'][0]['scoredDown'], 1)

      assertEqual(challenge_habit['history'][1]['scoredUp'], 1)
      assertEqual(challenge_habit['history'][1]['scoredDown'], 0)
    '';
  }
]
