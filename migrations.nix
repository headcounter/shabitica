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
  { file = "archive/2018/20180811_inboxOutsideUser.js";
    testScript = ''
      spec = getuser('bar', api_version=3)

      messages = spec.api.inbox.messages.get(
        page=0, conversation=getuser('foo').apiUser
      )
      assertEqual(len(messages), 1)
      assertIn('text', messages[0])
      assertEqual(messages[0]['text'], "Hello Bar!")

      spec = getuser('bar', api_version=4)

      user_record = spec.api.user.get()
      assertIn('inbox', user_record)
      assertNotIn('messages', user_record['inbox'])
    '';
  }
  { file = "archive/2019/20190131_habit_birthday.js";
    testScript = ''
      spec = getuser('birthday2019')
      user = spec.api.user.get()

      assertIn('items', user)
      assertIn('gear', user['items'])
      assertIn('owned', user['items']['gear'])
      assertIn('armor_special_birthday', user['items']['gear']['owned'])
      assertFalse(user['items']['gear']['owned']['armor_special_birthday'])

      assertIn('achievements', user)
      assertIn('habitBirthdays', user['achievements'])
      assertEqual(user['achievements']['habitBirthdays'], 1)

      spec = getuser('foo')
      user = spec.api.user.get()

      assertIn('items', user)
      assertIn('gear', user['items'])
      assertIn('owned', user['items']['gear'])
      assertNotIn('armor_special_birthday', user['items']['gear']['owned'])

      assertIn('achievements', user)
      assertNotIn('habitBirthdays', user['achievements'])
    '';
  }
  { file = "users/pi-day.js";
    testScript = ''
      spec = getuser('piday2019');
      user = spec.api.user.get()

      assertIn('items', user)
      assertIn('gear', user['items'])
      assertIn('owned', user['items']['gear'])

      assertIn('head_special_piDay', user['items']['gear']['owned'])
      assertFalse(user['items']['gear']['owned']['head_special_piDay'])

      assertIn('shield_special_piDay', user['items']['gear']['owned'])
      assertFalse(user['items']['gear']['owned']['shield_special_piDay'])
    '';
  }
  { file = "archive/2019/20190530_halfmoon_glasses.js";
    testScript = ''
      ineligible_user = getuser('piday2019').api.user.get()
      halfmoon_user = getuser('halfmoon').api.user.get()

      for user in [ineligible_user, halfmoon_user]:
        assertIn('items', user)
        assertIn('gear', user['items'])
        assertIn('owned', user['items']['gear'])

      for color in ['black', 'blue', 'green', 'pink', 'red', 'white',
                    'yellow']:
        item_name = 'eyewear_special_' + color + 'HalfMoon'

        assertIn(item_name, halfmoon_user['items']['gear']['owned'])
        assertTrue(halfmoon_user['items']['gear']['owned'][item_name])

        assertNotIn(item_name, ineligible_user['items']['gear']['owned'])
    '';
  }
  { file = "archive/2019/20190618_summer_splash_orcas.js";
    testScript = ''
      ineligible_user = getuser('halfmoon').api.user.get()
      orca_user = getuser('orca').api.user.get()

      for user in [ineligible_user, orca_user]:
        assertIn('items', user)
        assertIn('pets', user['items'])
        assertIn('mounts', user['items'])

      assertNotIn('Orca-Base', ineligible_user['items']['pets'])
      assertNotIn('Orca-Base', ineligible_user['items']['mounts'])

      assertIn('Orca-Base', orca_user['items']['mounts'])
      assertTrue(orca_user['items']['mounts']['Orca-Base'])
      assertNotIn('Orca-Base', orca_user['items']['pets'])
    '';
  }
]
