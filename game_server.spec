{alias, game_server, "./test/"}.
{logdir, "./test/logs"}.

{suites, game_server, all}.

%%{skip_cases, game_server, game_server_SUITE, register_test_case,
  %%  "This case fails, investigate later"}.
