-module(nova_plugins).
-export([
         build_index/1
        ]).




build_index(BaseOutputFile) ->
    Path = code:lib_dir(nova_plugins, priv),
    {ok, Filenames} = file:list_dir(Path),
    Index = build_index(Path, Filenames),
    Binary = json:encode(Index, [maps, binary]),
    Markdown = build_markdown(Index),
    io:format("Finished writing index. Indexed ~p modules~n", [length(Index)]),
    file:write_file(BaseOutputFile ++ ".json", [Binary]),
    file:write_file(BaseOutputFile ++ ".md", [Markdown]).

build_index(_Basepath, []) -> [];
build_index(Basepath, [Filename|Tl]) ->
    Filename0 = filename:join(Basepath, Filename),
    case filelib:is_dir(Filename0) of
        false ->
            %% This is a file - let's see if we can compile it and extract the
            %% metadata from it.
            {ok, Modulename, Binary} = compile:file(Filename0, [binary, report_errors]),
            {module, Module} = code:load_binary(Modulename, Filename, Binary),
            case erlang:function_exported(Module, plugin_info, 0) of
                true ->
                    Info = Module:plugin_info(),
                    [transform_info(Info)|build_index(Basepath, Tl)];
                _ ->
                    build_index(Basepath, Tl)
            end;
        _Else ->
            {ok, Filenames} = file:list_dir(Filename0),
            build_index(Filename0, Filenames) ++ build_index(Basepath, Tl)
    end.

build_markdown([]) -> [];
build_markdown([Info|Tl]) ->
    #{<<"name">> := Name,
      <<"version">> := Version,
      <<"description">> := Description,
      <<"author">> := Author,
      <<"options">> := Options} = Info,
    MD = <<"### ", Name/binary, "\n",
           "**Version**: ", Version/binary, "\n\n",
           "**Author**: ", Author/binary, "\n\n",
           "**Description**:\n\n",
           Description/binary, "\n\n">>,
    case Options of
        [] ->
            [<<MD/binary, "\n">>|build_markdown(Tl)];
        _ ->
            TableHeaders = <<"**Options**:\n\n| Parameter | Description |\n| --- | --- |\n">>,
            Options0 = generate_options(Options),
            [MD, TableHeaders, Options0, "\n\n"|build_markdown(Tl)]
    end.


generate_options([]) -> [];
generate_options([#{<<"key">> := Key, <<"description">> := Description}|Tl]) ->
    io:format("~p~n~p~n", [Key, Description]),
    KeyStr = erlang:atom_to_binary(Key),
    Line = <<"| ", KeyStr/binary, " | ", Description/binary, " |\n">>,
    [Line | generate_options(Tl)].

transform_info({Name, Version, Author, Description, Options}) ->
    #{<<"name">> => Name,
      <<"version">> => Version,
      <<"author">> => Author,
      <<"description">> => Description,
      <<"options">> => transform_options(Options)};
transform_info(Proplist) ->
    maps:from_list(Proplist).

transform_options([]) -> [];
transform_options([{Key, Description}|Tl]) ->
    [#{
       <<"key">> => Key,
       <<"description">> => Description
     }|transform_options(Tl)].
