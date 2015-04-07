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

-define(BASE_SECONDS, 62167219200).

%% @doc
%% Get Now with format `{MegaSecs, Secs, MicroSecs}'
%% @end
-spec now() -> erlang:timestamp().
now() ->
    os:timestamp().

%% @doc
%% Get Now timestamp
%% @end
-spec timestamp() -> integer().
timestamp() ->
    unix_timestamp(arrow:now()).

%% @doc
%% Get timestamp of given time
%% @end
-spec timestamp(arrow_datetime()) -> integer().
timestamp(Datetime) ->
    unix_timestamp(arrow:get(Datetime)).

%% @doc
%% Format given time to format `YYYY-MM-DD HH:mm:ss'
%% @end
-spec format(Timestamp :: erlang:timestamp()) -> DateString :: nonempty_string();
            (Datetime :: calendar:datetime()) -> DateString :: nonempty_string().
format(Timestamp) when is_integer(Timestamp)->
    format(unix_timestamp_to_datetime(Timestamp));

format({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    Text = io_lib:fwrite("~4..0b-~2..0b-~2..0b ~2..0b:~2..0b:~2..0b",
        [Year, Month, Day, Hour, Minute, Second]
    ),
    lists:flatten(Text).


%% @doc
%% Get Current time as format `{{Year, Month, Day}, {Hour, Minute, Second}}'
%% @end
-spec get() -> calendar:datetime().
get() ->
    calendar:universal_time().

%% @doc
%% Parse given time to format `{{Year, Month, Day}, {Hour, Minute, Second}}'
%% @end
-spec get(arrow_datetime()) -> calendar:datetime().
get(Timestamp) when is_integer(Timestamp) ->
    unix_timestamp_to_datetime(Timestamp);

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
-spec diff(D1 :: arrow_datetime(), D2 :: arrow_datetime()) -> DiffSeconds :: integer().
diff(D1, D2) ->
    D1Seconds = timestamp(D1),
    D2Seconds = timestamp(D2),
    calendar:datetime_to_gregorian_seconds(D1Seconds) - calendar:datetime_to_gregorian_seconds(D2Seconds).


%% @doc
%% Compare D1 and D2.
%% @end
-spec compare(D1 :: arrow_datetime(), D2 :: arrow_datetime()) -> Result :: arrow_compare().
compare(D1, D2) ->
    case diff(D1, D2) of
        N when N == 0 -> 0;
        N when N > 0 -> 1;
        N when N < 0 -> -1
    end.

%% @doc
%% Check whether D2 or D2Range is in D1Range.
%% @end
-spec in(arrow_range(), arrow_datetime()) -> boolean();
        (arrow_range(), arrow_range()) -> boolean().
in({_D1Start, _D1End} = D1, {D2Start, D2End}) ->
    in(D1, D2Start) andalso in(D1, D2End);

in({D1Start, D1End}, D2) ->
    CompareWithStart = compare(D2, D1Start),
    ComareWithEnd = compare(D2, D1End),
    CompareWithStart >= 0 andalso ComareWithEnd =< 0.


%% @doc
%% Add Years to Input Datetime
%% @end
add_years({{Year, Month, Day}, _Time}, Years) ->
    {{Year+ Years, Month, Day}, _Time}.

%% @doc
%% Add Months to Input Datetime
%% @end
add_months({{Year, Month, _Day}, _Time} = Datetime, Months) ->
    AddDay = do_add_months(Months, 0, {Year, Month, 0}),
    add_days(Datetime, AddDay).

%% @doc
%% Add Days to Input Datetime
%% @end
add_days({Date, Time}, Days) ->
    TotalDays = calendar:date_to_gregorian_days(Date) + Days,
    NewDate = calendar:gregorian_days_to_date(TotalDays),
    {NewDate, Time}.

%% @doc
%% Add Hours to Input Datetime
%% @end
add_hours(Datetime, Hours) ->
    add_seconds(Datetime, Hours*3600).

%% @doc
%% Add Minutes to Input Datetime
%% @end
add_minutes(Datetime, Minutes) ->
    add_seconds(Datetime, Minutes*60).

%% @doc
%% Add Seconds to Input Datetime
%% @end
add_seconds(Datetime, AddSecond) ->
    TotalSeconds = calendar:datetime_to_gregorian_seconds(Datetime) + AddSecond,
    calendar:gregorian_seconds_to_datetime(TotalSeconds).

%%% ====================================================
%%% Interal Functions
%%% ====================================================

-spec unix_timestamp(erlang:timestamp()) -> integer();
                    (calendar:datetime()) -> integer().
unix_timestamp({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs;

unix_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) - ?BASE_SECONDS.

-spec unix_timestamp_to_datetime(integer()) -> calendar:datetime() .
unix_timestamp_to_datetime(Timestamp) ->
    calendar:gregorian_seconds_to_datetime(Timestamp + ?BASE_SECONDS).


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






%%% ====================================================
%%% Tests
%%% ====================================================

-include_lib("eunit/include/eunit.hrl").
timestamp_test_() ->
    [
        ?_assertEqual(timestamp({{1970, 1, 1}, {0, 0, 0}}), 0),
        ?_assertEqual(timestamp({{2000, 1, 1}, {1, 1, 1}}), 946688461),
        ?_assertEqual(timestamp({{2015, 3, 26}, {11, 33, 0}}), 1427369580),
        ?_assertEqual(timestamp({{4444, 4, 4}, {4, 44, 44}}), 78080042684),
        ?_assertEqual(timestamp({{9999, 12, 31}, {23, 59, 59}}), 253402300799),
        ?_assertEqual(timestamp({{999, 12, 31}, {23, 59, 59}}), -30610224001),
        ?_assertEqual(timestamp({{1, 1, 1}, {0, 0, 0}}), -62135596800)
    ].


format_test_() ->
    [
        ?_assertEqual(format({{1970, 1, 1}, {0, 0, 0}}), "1970-01-01 00:00:00"),
        ?_assertEqual(format({{2000, 1, 1}, {1, 1, 1}}), "2000-01-01 01:01:01"),
        ?_assertEqual(format({{9999, 12, 31}, {23, 59, 59}}), "9999-12-31 23:59:59"),
        ?_assertEqual(format({{1, 1, 1}, {0, 0, 0}}), "0001-01-01 00:00:00"),

        ?_assertEqual(format(0), "1970-01-01 00:00:00"),
        ?_assertEqual(format(946688461), "2000-01-01 01:01:01"),
        ?_assertEqual(format(253402300799), "9999-12-31 23:59:59"),
        ?_assertEqual(format(-62135596800), "0001-01-01 00:00:00")
    ].

get_test_() ->
    [
        ?_assertEqual(arrow:get(0), {{1970, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:get(946688461), {{2000, 1, 1}, {1, 1, 1}}),
        ?_assertEqual(arrow:get(253402300799), {{9999, 12, 31}, {23, 59, 59}}),
        ?_assertEqual(arrow:get(-62135596800), {{1, 1, 1}, {0, 0, 0}}),

        ?_assertEqual(arrow:get("1970-01-01 00:00:00"), {{1970, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:get("2000-01-01 01:01:01"), {{2000, 1, 1}, {1, 1, 1}}),
        ?_assertEqual(arrow:get("9999-12-31 23:59:59"), {{9999, 12, 31}, {23, 59, 59}}),

        ?_assertEqual(arrow:get(<<"0001-01-01 00:00:00">>), {{1, 1, 1}, {0, 0, 0}}),

        ?_assertEqual(arrow:get({{1, 1, 1}, {0, 0, 0}}), {{1, 1, 1}, {0, 0, 0}})
    ].


add_years_test_() ->
    [
        ?_assertEqual(add_years({{1970, 1, 1}, {0, 0, 0}}, 0), {{1970, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(add_years({{1970, 1, 1}, {0, 0, 0}}, 100), {{2070, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(add_years({{2015, 1, 1}, {0, 0, 0}}, 1000), {{3015, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(add_years({{1970, 1, 1}, {0, 0, 0}}, -100), {{1870, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(add_years({{2015, 1, 1}, {0, 0, 0}}, -1000), {{1015, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(add_years({{2345, 1, 1}, {0, 0, 0}}, -345), {{2000, 1, 1}, {0, 0, 0}})
    ].

add_months_test_() ->
    [
        ?_assertEqual(add_months({{1970, 1, 1}, {0, 0, 0}}, 0), {{1970, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(add_months({{1970, 1, 1}, {0, 0, 0}}, 100), {{1978, 5, 1}, {0, 0, 0}}),
        ?_assertEqual(add_months({{1970, 1, 1}, {0, 0, 0}}, 567), {{2017, 4, 1}, {0, 0, 0}}),
        ?_assertEqual(add_months({{2015, 3, 26}, {0, 0, 0}}, 9999), {{2848, 6, 26}, {0, 0, 0}}),
        ?_assertEqual(add_months({{2015, 3, 26}, {0, 0, 0}}, -67), {{2009, 8, 26}, {0, 0, 0}}),
        ?_assertEqual(add_months({{2015, 3, 31}, {0, 0, 0}}, -1), {{2015, 2, 28}, {0, 0, 0}}),
        ?_assertEqual(add_months({{2015, 3, 31}, {0, 0, 0}}, -181), {{2000, 2, 29}, {0, 0, 0}}),
        ?_assertEqual(add_months({{2015, 3, 31}, {0, 0, 0}}, -9999), {{1181, 12, 31}, {0, 0, 0}})
    ].

add_days_test_() ->
    [
        ?_assertEqual(add_days({{1970, 1, 1}, {0, 0, 0}}, 0), {{1970, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(add_days({{1970, 1, 1}, {0, 0, 0}}, 100), {{1970, 4, 11}, {0, 0, 0}}),
        ?_assertEqual(add_days({{2015, 3, 26}, {0, 0, 0}}, 9999), {{2042, 8, 10}, {0, 0, 0}}),
        ?_assertEqual(add_days({{2015, 3, 1}, {0, 0, 0}}, -1), {{2015, 2, 28}, {0, 0, 0}}),
        ?_assertEqual(add_days({{2015, 3, 31}, {0, 0, 0}}, -181), {{2014, 10, 1}, {0, 0, 0}}),
        ?_assertEqual(add_days({{2015, 3, 31}, {0, 0, 0}}, -9999), {{1987, 11, 14}, {0, 0, 0}})
    ].
