# Arrow

erlang datetime unit build on `calender` module

## Types

--------------------|-------------------
arrow_compare()     |   `-1 | 0 | 1`
arrow_datetime()    |   integer() | nonempty_string() | binary() | calendar:datetime()
arrow_range()       |   {arrow_datetime(), arrow_datetime()}


## Functions

#### now/0
`now() -> erlang:timestamp()`

Get Current Datetime with format `{MegaSecs, Secs, MicroSecs}`.


#### timestamp/0
`timestamp() -> integer()`

Get Current Timestamp.

#### timestamp/1
`timestamp(arrow_datetime()) -> integer()`

Get Timestamp of Given datetime


#### format/1
`format(arrow_datetime()) -> nonempty_string()`

#### get/0
`get() -> calendar:datetime()`

Get Current Datetime with format `{{Year, Month, Day}, {Hour, Minute, Second}}`.

#### get/1
`get(arrow_datetime()) -> calendar:datetime()`

Get Input `Datetime::arrow_datetime()` with format `{{Year, Month, Day}, {Hour, Minute, Second}}`.

#### diff/2
`diff(D1::arrow_datetime(), D2::arrow_datetime()) -> DiffSeconds::integer()`

Diff seconds of D1 - D2.

#### compare/2
`compare(D1::arrow_datetime(), D2::arrow_datetime()) -> Result::arrow_compare()`

Compare D1 and D2.

#### in/2
`in(D1Range::arrow_range(), D2::arrow_datetime()) -> boolean()`

Check whether D2 in D1Range.

#### add_years/2
`add_years(Datetime::arrow_datetime(), Years::integer()) -> calendar:datetime()`

Add Years to Datetime

#### add_months/2
`add_months(Datetime::arrow_datetime(), Months::integer()) -> calendar:datetime()`

Add Months to Datetime

#### add_days/2
`add_days(Datetime::arrow_datetime(), Days::integer()) -> calendar:datetime()`

Add Days to Datetime

#### add_hours/2
`add_hours(Datetime::arrow_datetime(), Hours::integer()) -> calendar:datetime()`

Add Hours to Datetime

#### add_minutes/2
`add_minutes(Datetime::arrow_datetime(), Minutes::integer()) -> calendar:datetime()`

Add Minutes to Datetime

#### add_seconds/2
`add_seconds(Datetime::arrow_datetime(), Seconds::integer()) -> calendar:datetime()`

Add Seconds to Datetime


## Example

```erlang
{{Year, Month, Day}, {Hour, Minute, Second}} = arrow:get().
CurrentTimestamp = arrow:timestamp().
946688461 = arrow:timestamp({{2000, 1, 1}, {1, 1, 1}}).
946688461 = arrow:timestamp("2000-01-01 01:01:01").
"1970-01-01 00:00:00" = arrow:format({{1970, 1, 1}, {0, 0, 0}}).
"1970-01-01 00:00:00" = arrow:format(0).
{{2000, 1, 1}, {1, 1, 1}} = arrow:get(946688461).
{{9999, 12, 31}, {23, 59, 59}} = arrow:get("9999-12-31 23:59:59").
-31536000 = arrow:diff({{1970, 1, 1}, {0, 0, 0}}, {{1971, 1, 1}, {0, 0, 0}}).
-11837268671 = arrow:diff({{1970, 1, 1}, {0, 0, 0}}, "2345-02-09 10:11:11").
0 = arrow:compare({{1970, 1, 1}, {0, 0, 0}}, 0).
1 = arrow:compare({{1970, 1, 1}, {0, 0, 0}}, <<"1960-01-01 00:00:00">>).
true = arrow:in({ {{1970, 1, 1}, {0, 0, 0}}, 100 }, "1970-01-01 00:00:10").
false = arrow:in({ "1970-01-01 00:00:00", <<"2000-01-01 00:00:00">> }, 955127654).
{{2000, 1, 1}, {0, 0, 0}} = arrow:add_years("2345-01-01 00:00:00", -345).
{{2848, 6, 26}, {0, 0, 0}} = arrow:add_months({{2015, 3, 26}, {0, 0, 0}}, 9999).
{{1987, 11, 14}, {0, 0, 0}} = arrow:add_days(1427760000, -9999).
{{1901, 3, 2}, {9, 0, 50}} = arrow:add_hours(<<"2015-03-31 00:00:50">>, -999999).
{{2015, 4, 1}, {22, 39, 0}} = arrow:add_minutes({{2015, 3, 26}, {0, 0, 0}}, 9999).
{{1970, 1, 1}, {0, 1, 40}} = arrow:add_seconds({{1970, 1, 1}, {0, 0, 0}}, 100).
```