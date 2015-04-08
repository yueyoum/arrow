%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-03-23 14:46
%%%-------------------------------------------------------------------
-module(arrow).
-author("wang").

%% API
-export([now/0,
         timestamp/0,
         timestamp/1,
         format/1,
         get/0,
         get/1,
         diff/2,
         compare/2,
         in/2,
         add_years/2,
         add_months/2,
         add_days/2,
         add_hours/2,
         add_minutes/2,
         add_seconds/2]).



-type arrow_datetime() :: integer() | nonempty_string() | binary() | calendar:datetime().
-type arrow_range() :: {arrow_datetime(), arrow_datetime()}.
-type arrow_compare() :: -1 | 0 | 1.

-export_type([arrow_datetime/0, arrow_range/0, arrow_compare/0]).

-define(BASE_SECONDS, 62167219200).

%% @doc
%% Get Now with format `{MegaSecs, Secs, MicroSecs}'
%% @end
-spec now() -> Return when
        Return  :: erlang:timestamp().
now() ->
    os:timestamp().

%% @doc
%% Get Now timestamp
%% @end
-spec timestamp() -> UnixTimestamp when
        UnixTimestamp   :: integer().
timestamp() ->
    unix_timestamp(arrow:now()).

%% @doc
%% Get timestamp of given time
%% @end
-spec timestamp(Input) -> UnixTimestamp when
        Input           :: arrow_datetime(),
        UnixTimestamp   :: integer().
timestamp(Datetime) ->
    unix_timestamp(arrow:get(Datetime)).

%% @doc
%% Format given time to format `YYYY-MM-DD HH:mm:ss'
%% @end
-spec format(Input) -> DateString when
        Input           :: arrow_datetime(),
        DateString      :: nonempty_string().
format(Input) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = arrow:get(Input),
    Text = io_lib:fwrite("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b",
        [Year, Month, Day, Hour, Minute, Second]
    ),
    lists:flatten(Text).


%% @doc
%% Get Current time as format `{{Year, Month, Day}, {Hour, Minute, Second}}'
%% @end
-spec get() -> Datetime when
        Datetime    :: calendar:datetime().
get() ->
    calendar:universal_time().

%% @doc
%% Parse given time to format `{{Year, Month, Day}, {Hour, Minute, Second}}'
%% @end
-spec get(Input) -> Datetime when
        Input       :: arrow_datetime(),
        Datetime    :: calendar:datetime().
get(UnixTimestamp) when is_integer(UnixTimestamp) ->
    unix_timestamp_to_datetime(UnixTimestamp);

get(DateString) when is_list(DateString) andalso length(DateString) == 19 ->
    {ok, [Year, Month, Day, Hour, Minute, Second], []} = io_lib:fread("~d-~d-~d ~d:~d:~d", DateString),
    {{Year, Month, Day}, {Hour, Minute, Second}};

get(DateBinary) when is_binary(DateBinary) ->
    arrow:get(binary_to_list(DateBinary));

get({{_, _, _}, {_, _, _}} = Datetime) ->
    Datetime.

%% @doc
%% Diff seconds of D1 and D2. `D1 - D2'
%% @end
-spec diff(D1, D2) -> DiffSeconds when
        D1              :: arrow_datetime(),
        D2              :: arrow_datetime(),
        DiffSeconds     :: integer().
diff(D1, D2) ->
    D1Seconds = timestamp(D1),
    D2Seconds = timestamp(D2),
    D1Seconds - D2Seconds.


%% @doc
%% Compare D1 and D2.
%% @end
-spec compare(D1, D2) -> CompareResult when
        D1              :: arrow_datetime(),
        D2              :: arrow_datetime(),
        CompareResult   :: arrow_compare().
compare(D1, D2) ->
    case diff(D1, D2) of
        N when N == 0 -> 0;
        N when N > 0 -> 1;
        N when N < 0 -> -1
    end.

%% @doc
%% Check whether D2 or D2Range is in D1Range.
%% @end
-spec in(D1Range, D2) -> Result when
        D1Range         :: arrow_range(),
        D2              :: arrow_datetime(),
        Result          :: boolean().
in({D1Start, D1End}, D2) ->
    CompareWithStart = compare(D2, D1Start),
    ComareWithEnd = compare(D2, D1End),
    CompareWithStart >= 0 andalso ComareWithEnd =< 0.


%% @doc
%% Add Years to Input Datetime
%% @end
-spec add_years(Input, Years) -> Datetime when
        Input           :: arrow_datetime(),
        Years           :: integer(),
        Datetime        :: calendar:datetime().
add_years(Datetime, Years) ->
    {{Year, Month, Day}, Time} = arrow:get(Datetime),
    {{Year + Years, Month, Day}, Time}.

%% @doc
%% Add Months to Input Datetime
%% @end
-spec add_months(Input, Months) -> Datetime when
        Input           :: arrow_datetime(),
        Months          :: integer(),
        Datetime        :: calendar:datetime().
add_months(Datetime, Months) ->
    {{Year, Month, _Day}, _Time} = arrow:get(Datetime),
    AddDay = do_add_months(Months, 0, {Year, Month, 0}),
    add_days(Datetime, AddDay).

%% @doc
%% Add Days to Input Datetime
%% @end
-spec add_days(Input, Days) -> Datetime when
        Input           :: arrow_datetime(),
        Days            :: integer(),
        Datetime        :: calendar:datetime().
add_days(Datetime, Days) ->
    {Date, Time} = arrow:get(Datetime),
    TotalDays = calendar:date_to_gregorian_days(Date) + Days,
    NewDate = calendar:gregorian_days_to_date(TotalDays),
    {NewDate, Time}.

%% @doc
%% Add Hours to Input Datetime
%% @end
-spec add_hours(Input, Hours) -> Datetime when
        Input           :: arrow_datetime(),
        Hours           :: integer(),
        Datetime        :: calendar:datetime().
add_hours(Datetime, Hours) ->
    add_seconds(Datetime, Hours*3600).

%% @doc
%% Add Minutes to Input Datetime
%% @end
-spec add_minutes(Input, Minutes) -> Datetime when
        Input           :: arrow_datetime(),
        Minutes         :: integer(),
        Datetime        :: calendar:datetime().
add_minutes(Datetime, Minutes) ->
    add_seconds(Datetime, Minutes*60).

%% @doc
%% Add Seconds to Input Datetime
%% @end
-spec add_seconds(Input, Seconds) -> Datetime when
        Input           :: arrow_datetime(),
        Seconds         :: integer(),
        Datetime        :: calendar:datetime().
add_seconds(Datetime, Seconds) ->
    DatetimeNormalized = arrow:get(Datetime),
    TotalSeconds = calendar:datetime_to_gregorian_seconds(DatetimeNormalized) + Seconds,
    calendar:gregorian_seconds_to_datetime(TotalSeconds).

%%% ====================================================
%%% Interal Functions
%%% ====================================================

-spec unix_timestamp(Input) -> UnixTimestamp when
        Input           :: erlang:timestamp() | calendar:datetime(),
        UnixTimestamp   :: integer().
unix_timestamp({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs;

unix_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?BASE_SECONDS.

-spec unix_timestamp_to_datetime(UnixTimestamp) -> Datetime when
        UnixTimestamp   :: integer(),
        Datetime        :: calendar:datetime().
unix_timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp + ?BASE_SECONDS).

-spec do_add_months(AddMonth, Acc, {Year, Month, AddDays}) -> AddDaysResult when
        AddMonth        :: integer(),
        Acc             :: integer(),
        Year            :: integer(),
        Month           :: 1..12,
        AddDays         :: integer(),
        AddDaysResult   :: integer().
do_add_months(AddMonth, Acc, {_, _, AddDays}) when AddMonth == Acc ->
    AddDays;

do_add_months(AddMonth, Acc, {Year, Month, AddDays}) when AddMonth < Acc ->
    {NewYear, NewMonth} =
        case Month - 1 < 1 of
            true ->
                {Year-1, 12};
            false ->
                {Year, Month-1}
        end,

    do_add_months(AddMonth, Acc-1, {NewYear, NewMonth, AddDays-calendar:last_day_of_the_month(Year, Month)});

do_add_months(AddMonth, Acc, {Year, Month, AddDays}) when AddMonth > Acc ->
    {NewYear, NewMonth} =
    case Month + 1 > 12 of
        true ->
            {Year+1, 1};
        false ->
            {Year, Month+1}
    end,

    do_add_months(AddMonth, Acc+1, {NewYear, NewMonth, AddDays+calendar:last_day_of_the_month(Year, Month)}).

