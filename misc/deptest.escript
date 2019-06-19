#!/usr/bin/env escript

%% A hack for running dependencies test, see Makefile#deptest.  Adds riak’s _build as
%% a base_dir to the test profile, renames test profile to deptest

%% E. g. FileIn=rebar.config, FileOut=rebar.config.deptest
main([FileIn,FileOut]) ->
    case file:consult(FileIn) of
        {ok,Conf0} -> ok;
        _ -> Conf0=[]          % no rebar.config! amazing
    end,
    Conf = lists:ukeymerge(1, lists:keysort(1,Conf0), [{profiles,[]}]),
    [Profiles] = [ P || {profiles,P} <- Conf ],
    
    %% if no test profile, add an empty one
    ProfilesT = lists:ukeymerge(1, lists:keysort(1,Profiles), [{test,[]}]),
    
    NewProfiles
        = [ case P of
                %% rename test → deptest; add base_dir
                {test,Spec} -> {deptest, [{base_dir,"../../.."} | Spec]};
                X -> X
            end
            || P <- ProfilesT ],

    NewConf = lists:keyreplace(profiles, 1, Conf, {profiles,NewProfiles}),
    file:write_file(FileOut, [io_lib:format("~tp.~n", [X]) || X <- NewConf]).


