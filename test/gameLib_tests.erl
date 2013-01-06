-module(gameLib_tests).

-include_lib("eunit/include/eunit.hrl").

encode_game_command_test_()->
    [
     {"encode some data"
     ,fun encode_some_data_tst/0}
     ,{"encode command reply -> {ok, registered}"
       ,fun encode_register_command_reply/0}
     ,{"encode list_accounts command -> ListOfNames"
       ,fun encode_list_accounts_command_reply/0}
     ,{"encode empty list -> []"
       ,fun encode_empty_list/0}
    ].

decode_game_command_test_()->
    [
     {"decode some binary data"
      ,fun decode_some_binary/0}
     %% ,{"decode register command with name and password"
     %%   ,fun decode_register_command_binary/0}
     %% ,{"decode empty binary"
     %%   ,fun decode_empty_binary/0}
     %% ,{"decode undefined command binary data"
     %%   ,fun decode_undefined_command_binary/0}
    ].

%% encode tst
encode_some_data_tst()->
    SomeData = "Some Data",
    EncodedData = <<131,107,0,9,83,111,109,101,32,68,97,116,97>>,
    ?assertEqual(EncodedData, gameLib:encode(SomeData)).
    
encode_register_command_reply()->
    RegisterReply = "ok, registered",
    EncodedReply = <<131,107,0,14,111,107,44,32,114,101,103,105,115,116,101,114,101,100>>,
    ?assertEqual(EncodedReply, gameLib:encode(RegisterReply)).

encode_list_accounts_command_reply()->
    Name1 = "name1",
    Name2 = "name2",
    ListOfNames = [Name1, Name2],
    EncodedList = <<131,108,0,0,0,2,107,0,5,110,97,109,101,49,107,0,5,110,97,109,101,50,106>>,
    ?assertEqual(EncodedList, gameLib:encode(ListOfNames)).

encode_empty_list()->
    EmptyList = [],
    EncodedList = <<131,106>>,
    ?assertEqual(EncodedList, gameLib:encode(EmptyList)).

%% decode tsts
decode_some_binary()->
    SomeBinary = <<131,107,0,9,83,111,109,101,32,68,97,116,97>>,
    DecodedData = "Some Data",
    ?assertEqual(DecodedData, gameLib:decode(SomeBinary)).

%% decode_register_command_binary()->
%%     BinaryRegisterCommand = <<"register, name, password">>,
%%     DecodedRegisterCommand = {"register", "name", "password"},  
%%     ?assertEqual(DecodedRegisterCommand,gameLib:decode(BinaryRegisterCommand)).

%% decode_empty_binary()->
%%     EmptyCommandBinary = <<>>,
%%     DecodedEmptyCommand = {},
%%     ?assertEqual(EmptyCommandBinary,gameLib:decode(DecodedEmptyCommand)).

%% decode_undefined_command_binary()->
%%     UndefinedCommandBinary = <<"undefined_command">>,
%%     DecodedUndefinedCommand = {"undefined_command"},
%%     ?assertEqual(DecodedUndefinedCommand,gameLib:decode(UndefinedCommandBinary)).
