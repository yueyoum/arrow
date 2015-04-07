%%%-------------------------------------------------------------------
%%% @author wang <yueyoum@gmail.com>
%%% @copyright (C) 2015
%%% @doc
%%%
%%% @end
%%% Created : 2015-04-07 17:52
%%%-------------------------------------------------------------------
-module(arrow_tests).
-author("wang").

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
timestamp_test_() ->
    [
        ?_assertEqual(arrow:timestamp({{1970, 1, 1}, {0, 0, 0}}), 0),
        ?_assertEqual(arrow:timestamp({{2000, 1, 1}, {1, 1, 1}}), 946688461),
        ?_assertEqual(arrow:timestamp({{2015, 3, 26}, {11, 33, 0}}), 1427369580),
        ?_assertEqual(arrow:timestamp({{4444, 4, 4}, {4, 44, 44}}), 78080042684),
        ?_assertEqual(arrow:timestamp({{9999, 12, 31}, {23, 59, 59}}), 253402300799),
        ?_assertEqual(arrow:timestamp({{999, 12, 31}, {23, 59, 59}}), -30610224001),
        ?_assertEqual(arrow:timestamp({{1, 1, 1}, {0, 0, 0}}), -62135596800),

        ?_assertEqual(arrow:timestamp("2000-01-01 01:01:01"), 946688461)
    ].


format_test_() ->
    [
        ?_assertEqual(arrow:format({{1970, 1, 1}, {0, 0, 0}}), "1970-01-01 00:00:00"),
        ?_assertEqual(arrow:format({{2000, 1, 1}, {1, 1, 1}}), "2000-01-01 01:01:01"),
        ?_assertEqual(arrow:format({{9999, 12, 31}, {23, 59, 59}}), "9999-12-31 23:59:59"),
        ?_assertEqual(arrow:format({{1, 1, 1}, {0, 0, 0}}), "0001-01-01 00:00:00"),

        ?_assertEqual(arrow:format(0), "1970-01-01 00:00:00"),
        ?_assertEqual(arrow:format(946688461), "2000-01-01 01:01:01"),
        ?_assertEqual(arrow:format(253402300799), "9999-12-31 23:59:59"),
        ?_assertEqual(arrow:format(-62135596800), "0001-01-01 00:00:00")
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
        ?_assertEqual(arrow:add_years({{1970, 1, 1}, {0, 0, 0}}, 0), {{1970, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_years({{1970, 1, 1}, {0, 0, 0}}, 100), {{2070, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_years({{2015, 1, 1}, {0, 0, 0}}, 1000), {{3015, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_years({{1970, 1, 1}, {0, 0, 0}}, -100), {{1870, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_years({{2015, 1, 1}, {0, 0, 0}}, -1000), {{1015, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_years({{2345, 1, 1}, {0, 0, 0}}, -345), {{2000, 1, 1}, {0, 0, 0}}),

        ?_assertEqual(arrow:add_years(11833862400, -345), {{2000, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_years("2345-01-01 00:00:00", -345), {{2000, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_years(<<"2345-01-01 00:00:00">>, -345), {{2000, 1, 1}, {0, 0, 0}})
    ].

add_months_test_() ->
    [
        ?_assertEqual(arrow:add_months({{1970, 1, 1}, {0, 0, 0}}, 0), {{1970, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_months({{1970, 1, 1}, {0, 0, 0}}, 100), {{1978, 5, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_months({{1970, 1, 1}, {0, 0, 0}}, 567), {{2017, 4, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_months({{2015, 3, 26}, {0, 0, 0}}, 9999), {{2848, 6, 26}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_months({{2015, 3, 26}, {0, 0, 0}}, -67), {{2009, 8, 26}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_months({{2015, 3, 31}, {0, 0, 0}}, -1), {{2015, 2, 28}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_months({{2015, 3, 31}, {0, 0, 0}}, -181), {{2000, 2, 29}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_months({{2015, 3, 31}, {0, 0, 0}}, -9999), {{1181, 12, 31}, {0, 0, 0}}),

        ?_assertEqual(arrow:add_months(1427760000, -9999), {{1181, 12, 31}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_months("2015-03-31 00:00:00", -9999), {{1181, 12, 31}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_months(<<"2015-03-31 00:00:00">>, -9999), {{1181, 12, 31}, {0, 0, 0}})
    ].

add_days_test_() ->
    [
        ?_assertEqual(arrow:add_days({{1970, 1, 1}, {0, 0, 0}}, 0), {{1970, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_days({{1970, 1, 1}, {0, 0, 0}}, 100), {{1970, 4, 11}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_days({{2015, 3, 26}, {0, 0, 0}}, 9999), {{2042, 8, 10}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_days({{2015, 3, 1}, {0, 0, 0}}, -1), {{2015, 2, 28}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_days({{2015, 3, 31}, {0, 0, 0}}, -181), {{2014, 10, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_days({{2015, 3, 31}, {0, 0, 0}}, -9999), {{1987, 11, 14}, {0, 0, 0}}),

        ?_assertEqual(arrow:add_days(1427760000, -9999), {{1987, 11, 14}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_days("2015-03-31 00:00:00", -9999), {{1987, 11, 14}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_days(<<"2015-03-31 00:00:00">>, -9999), {{1987, 11, 14}, {0, 0, 0}})
    ].

add_hours_test_() ->
    [
        ?_assertEqual(arrow:add_hours({{1970, 1, 1}, {0, 0, 0}}, 0), {{1970, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_hours({{1970, 1, 1}, {0, 0, 0}}, 100), {{1970, 1, 5}, {4, 0, 0}}),
        ?_assertEqual(arrow:add_hours({{1970, 1, 1}, {0, 0, 0}}, 567), {{1970, 1, 24}, {15, 0, 0}}),
        ?_assertEqual(arrow:add_hours({{2015, 3, 26}, {0, 0, 0}}, 9999), {{2016, 5, 15}, {15, 0, 0}}),
        ?_assertEqual(arrow:add_hours({{2015, 3, 26}, {0, 0, 0}}, -67), {{2015, 3, 23}, {5, 0, 0}}),
        ?_assertEqual(arrow:add_hours({{2015, 3, 31}, {0, 0, 0}}, -1), {{2015, 3, 30}, {23, 0, 0}}),
        ?_assertEqual(arrow:add_hours({{2015, 3, 31}, {0, 0, 0}}, -9999), {{2014, 2, 7}, {9, 0, 0}}),
        ?_assertEqual(arrow:add_hours({{2015, 3, 31}, {0, 0, 50}}, -999999), {{1901, 3, 2}, {9, 0, 50}}),

        ?_assertEqual(arrow:add_hours(1427760050, -999999), {{1901, 3, 2}, {9, 0, 50}}),
        ?_assertEqual(arrow:add_hours("2015-03-31 00:00:50", -999999), {{1901, 3, 2}, {9, 0, 50}}),
        ?_assertEqual(arrow:add_hours(<<"2015-03-31 00:00:50">>, -999999), {{1901, 3, 2}, {9, 0, 50}})
    ].

add_minutes_test_() ->
    [
        ?_assertEqual(arrow:add_minutes({{1970, 1, 1}, {0, 0, 0}}, 0), {{1970, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_minutes({{1970, 1, 1}, {0, 0, 0}}, 100), {{1970, 1, 1}, {1, 40, 0}}),
        ?_assertEqual(arrow:add_minutes({{1970, 1, 1}, {0, 0, 0}}, 567), {{1970, 1, 1}, {9, 27, 0}}),
        ?_assertEqual(arrow:add_minutes({{2015, 3, 26}, {0, 0, 0}}, 9999), {{2015, 4, 1}, {22, 39, 0}}),
        ?_assertEqual(arrow:add_minutes({{2015, 3, 26}, {0, 0, 0}}, -67), {{2015, 3, 25}, {22, 53, 0}}),
        ?_assertEqual(arrow:add_minutes({{2015, 3, 31}, {0, 0, 0}}, -1), {{2015, 3, 30}, {23, 59, 0}}),
        ?_assertEqual(arrow:add_minutes({{2015, 3, 31}, {0, 0, 0}}, -9999), {{2015, 3, 24}, {1, 21, 0}}),
        ?_assertEqual(arrow:add_minutes({{2015, 3, 31}, {0, 0, 50}}, -99999999), {{1825, 2, 10}, {13, 21, 50}}),

        ?_assertEqual(arrow:add_minutes(1427760050, -99999999), {{1825, 2, 10}, {13, 21, 50}}),
        ?_assertEqual(arrow:add_minutes("2015-03-31 00:00:50", -99999999), {{1825, 2, 10}, {13, 21, 50}}),
        ?_assertEqual(arrow:add_minutes(<<"2015-03-31 00:00:50">>, -99999999), {{1825, 2, 10}, {13, 21, 50}})
    ].

add_seconds_test_() ->
    [
        ?_assertEqual(arrow:add_seconds({{1970, 1, 1}, {0, 0, 0}}, 0), {{1970, 1, 1}, {0, 0, 0}}),
        ?_assertEqual(arrow:add_seconds({{1970, 1, 1}, {0, 0, 0}}, 100), {{1970, 1, 1}, {0, 1, 40}}),
        ?_assertEqual(arrow:add_seconds({{1970, 1, 1}, {0, 0, 0}}, 99999999), {{1973, 3, 3}, {9, 46, 39}}),
        ?_assertEqual(arrow:add_seconds({{1970, 1, 1}, {0, 0, 0}}, -99999999), {{1966, 10, 31}, {14, 13, 21}}),
        ?_assertEqual(arrow:add_seconds({{2015, 3, 31}, {0, 0, 0}}, -9999), {{2015, 3, 30}, {21, 13, 21}}),

        ?_assertEqual(arrow:add_seconds(1427760000, -9999), {{2015, 3, 30}, {21, 13, 21}}),
        ?_assertEqual(arrow:add_seconds("2015-03-31 00:00:00", -9999), {{2015, 3, 30}, {21, 13, 21}}),
        ?_assertEqual(arrow:add_seconds(<<"2015-03-31 00:00:00">>, -9999), {{2015, 3, 30}, {21, 13, 21}})
    ].

diff_test_() ->
    [
        ?_assertEqual(arrow:diff({{1970, 1, 1}, {0, 0, 0}}, {{1971, 1, 1}, {0, 0, 0}}), -31536000),
        ?_assertEqual(arrow:diff({{1970, 1, 1}, {0, 0, 0}}, "2345-02-09 10:11:11"), -11837268671),
        ?_assertEqual(arrow:diff({{1970, 1, 1}, {0, 0, 0}}, -4604342400), 4604342400),
        ?_assertEqual(arrow:diff(<<"2015-04-07 17:10:10">>, {{2000, 1, 1}, {9, 9, 9}}), 481708861)
    ].

compare_test_() ->
    [
        ?_assertEqual(arrow:compare({{1970, 1, 1}, {0, 0, 0}}, 0), 0),
        ?_assertEqual(arrow:compare({{1970, 1, 1}, {0, 0, 0}}, "1970-01-01 00:00:01"), -1),
        ?_assertEqual(arrow:compare({{1970, 1, 1}, {0, 0, 0}}, <<"1960-01-01 00:00:00">>), 1),
        ?_assertEqual(arrow:compare(1428426854, {{1970, 1, 1}, {0, 0, 0}}), 1)
    ].

in_test_() ->
    [
        ?_assertEqual(arrow:in({ {{1970, 1, 1}, {0, 0, 0}}, {{1970, 1, 1}, {0, 0, 0}} }, {{1970, 1, 1}, {0, 0, 0}}), true),
        ?_assertEqual(arrow:in({ {{1970, 1, 1}, {0, 0, 0}}, {{1970, 1, 2}, {0, 0, 0}} }, {{1970, 1, 3}, {0, 0, 0}}), false),
        ?_assertEqual(arrow:in({ {{1970, 1, 1}, {0, 0, 0}}, 100 }, "1970-01-01 00:00:10"), true),
        ?_assertEqual(arrow:in({ {{1970, 1, 1}, {0, 0, 0}}, 100 }, "1970-01-01 01:00:00"), false),
        ?_assertEqual(arrow:in({ "1970-01-01 00:00:00", <<"2000-01-01 00:00:00">> }, 100), true),
        ?_assertEqual(arrow:in({ "1970-01-01 00:00:00", <<"2000-01-01 00:00:00">> }, 955127654), false)
    ].

-endif.