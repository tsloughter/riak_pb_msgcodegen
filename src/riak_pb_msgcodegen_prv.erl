-module('riak_pb_msgcodegen_prv').

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'riak_pb_msgcodegen').
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 riak_pb_msgcodegen"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
                  undefined ->
                      rebar_state:project_apps(State);
                  AppInfo ->
                      [AppInfo]
              end,
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
         FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.csv\$"),

         CompileFun = fun(Source, _Opts1) ->
                          ModName = filename:basename(Source, ".csv"),
                          Target = ModName ++ ".erl",
                          generate(Source, filename:join(SourceDir, Target))
                      end,

         rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
     end || AppInfo <- Apps],

    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%

generate(CSV, Erl) ->
    Tuples = load_csv(CSV),
    ModName = filename:basename(CSV, ".csv"),
    Module = generate_module(ModName, Tuples),
    Formatted = erl_prettypr:format(Module),
    ok = file:write_file(Erl, Formatted),
    rebar_api:info("Generated ~s~n", [Erl]).

load_csv(SourceFile) ->
    {ok, Bin} = file:read_file(SourceFile),
    csv_to_tuples(unicode:characters_to_list(Bin, latin1)).

csv_to_tuples(String) ->
    Lines = string:tokens(String, [$\r,$\n]),
    [ begin
          [Code, Message, Proto] = string:tokens(Line, ","),
          {list_to_integer(Code), string:to_lower(Message), Proto ++ "_pb"}
      end
     || Line <- Lines].

generate_module(Name, Tuples) ->
    %% TODO: Add generated doc comment at the top
    Mod = erl_syntax:attribute(erl_syntax:atom(module),
                               [erl_syntax:atom(Name)]),
    ExportsList = [
                    erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(1))
                    || Fun <- [msg_type, msg_code, decoder_for] ],

    Exports = erl_syntax:attribute(erl_syntax:atom(export),
                                   [erl_syntax:list(ExportsList)]),

    Clauses = generate_msg_type(Tuples) ++
              generate_msg_code(Tuples) ++
              generate_decoder_for(Tuples),

    erl_syntax:form_list([Mod, Exports|Clauses]).

generate_decoder_for(Tuples) ->
    Spec = erl_syntax:text("-spec decoder_for(non_neg_integer()) -> module().\n"),
    Name = erl_syntax:atom(decoder_for),
    Clauses = [
                erl_syntax:clause([erl_syntax:integer(Code)],
                                  none,
                                  [erl_syntax:atom(Mod)])
                || {Code, _, Mod} <- Tuples ],
    [ Spec, erl_syntax:function(Name, Clauses) ].

generate_msg_code(Tuples) ->
    Spec = erl_syntax:text("-spec msg_code(atom()) -> non_neg_integer()."),
    Name = erl_syntax:atom(msg_code),
    Clauses = [
               erl_syntax:clause([erl_syntax:atom(Msg)], none, [erl_syntax:integer(Code)])
               || {Code, Msg, _} <- Tuples ],
    [ Spec, erl_syntax:function(Name, Clauses) ].

generate_msg_type(Tuples) ->
    Spec = erl_syntax:text("-spec msg_type(non_neg_integer()) -> atom()."),
    Name = erl_syntax:atom(msg_type),
    Clauses = [
               erl_syntax:clause([erl_syntax:integer(Code)], none, [erl_syntax:atom(Msg)])
               || {Code, Msg, _} <- Tuples ],
    CatchAll = erl_syntax:clause([erl_syntax:underscore()], none, [erl_syntax:atom(undefined)]),
    [ Spec, erl_syntax:function(Name, Clauses ++ [CatchAll]) ].
