#!/usr/bin/env escript

%% A hack for running dependencies test, see Makefile#deptest

main([FileIn,FileOut]) ->
    {ok,Conf0} = file:consult(FileIn),
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


