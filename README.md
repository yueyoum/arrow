# Arrow

erlang datetime unit

## Types

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

