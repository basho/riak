#Riak TS 1.2.0 Release Notes

Released February 23, 2016.

Riak TS 1.2.0 introduces riak_shell, a shell that allows you to run SQL within Riak TS. This release also expands aggregate and arithmetic functionality to allow them to be used together, enabling deeper analysis of time series data. 


##New Features

* riak_shell is a configurable, extendable shell for Riak that allows you to run SQL commands and logging in a single shell within Riak TS. You can find more information about riak_shell [here](http://docs.basho.com/riakts/1.2/using/riakshell/).


##Changes

* Aggregate and arithmetic functions can now be used together in a single value expression. [[PR #1327](https://github.com/basho/riak_kv/pull/1327), [PR #90](https://github.com/basho/riak_ql/pull/90), and [PR #95](https://github.com/basho/riak_ql/pull/95)]
* In Riak TS 1.1, `STDDEV()` was actually an implementation of Population Standard Deviation. Since it is standard in database systems to use `STDDEV()` for implementations of Sample Standard Deviation and `STDDEV_POP()` or similar for Population Standard Deviation, `STDDEV()` has been renamed `STDDEV_POP()`. Additionally, `STDDEV_SAMP` has been added for Sample Standard Deviation, and `STDDEV()` is now treated as Sample Standard Deviation as well. [[PR #98](https://github.com/basho/riak_ql/pull/98)]


##Bugfixes

* [[PR #95](https://github.com/basho/riak_ql/pull/95)] Semicolons are now valid statement terminators.
* [[PR #1338](https://github.com/basho/riak_kv/pull/1338)] If you divide by zero using `SELECT`, you will now receive an error. 


##Compatibility
Riak TS is compatible with the following operating systems:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS
* Ubuntu 14.04 LTS
* Debian 7 (development only)
* OSX 10.8+ (development only)


##Known Issues

* For security reasons, you should change the owner of the /etc/init.d/riak file to the root user after installation has completed. See our [product advisory](http://docs.basho.com/riak/2.1.3/community/product-advisories/codeinjectioninitfiles/) for more information and further instruction.
* Rolling upgrades are not supported.
* AAE must be turned off.
* Riak Search is not supported.
* Multi-Datacenter Replication is not supported.



#Riak TS 1.1.0 Release Notes

Released January 12, 2015.

This release builds on Riak TS 1.0 to enable further analysis of time series data with aggregates and arithmetic functionality.

##New Features
###Aggregations
In Riak TS 1.0.0 you could run a` WHERE` clause that returned a particular row set of time series data. In 1.1.0 you can apply a function (such as `COUNT`) in the `SELECT` clause that operates on those responses in aggregate.

For instance,

```
SELECT fun(X) FROM tablename WHERE
```

Where `fun` is one of:

* `SUM`
* `COUNT`
* `AVG` / `MEAN`
* `MIN`
* `MAX`
* `STDDEV`

And where is (X) is either a column name, or a multi-column expression avg(temperature/pressure).


###Arithmetic
Riak TS now also supports arithmetic operations in the `SELECT` list. The arithmetic operations available in this release are: Numeric Literals, Addition, Subtraction, Multiplication, Division, and Negation. 

* +
* -
* /
* *
* (
* )

For example,

```
SELECT 555, 1.1, 1e1, 1.123e-2 from GeoCheckin
WHERE time > 1452252523182 AND time < 1452252543182 AND myfamily = 'family1' AND myseries = 'series1'
```

>**Remember:** Arithmetic operations and aggregate functions cannot be mixed in a single value expression.

###Dynamic Schema Discovery
You can now query a table definition with the `DESCRIBE` table query which returns the table's rows and columns.

For example:

```sql
DESCRIBE GeoCheckin
```

Returns:
 (Rows and Columns)
```
Column      | Type      | Is Null | Partition Key | Local Key
--------------------------------------------------------
myfamily    | varchar   | false   | 1             | 1
myseries    | varchar   | false   | 2             | 2
time        | timestamp | false   | 3             | 3
weather     | varchar   | false   | <null>        | <null>
temperature | double    | false   | <null>        | <null>
```

###Create Tables via Clients
You can create tables using `CREATE TABLE`. Simply execute your `CREATE TABLE` command in a query, and it will be created. 

```sql 
CREATE TABLE GeoCheckin
(
   myfamily    varchar   not null,
   myseries    varchar   not null,
   time        timestamp not null,
   weather     varchar   not null,
   temperature double,
   PRIMARY KEY (
     (myfamily, myseries, quantum(time, 15, 'm')),
     myfamily, myseries, time
   )
)
```

A successful table creation will return nothing, and an exception response will be returned if the attempt was unsuccessful.

##Compatibility
Riak TS is compatible with the following operating systems:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS
* Ubuntu 14.04 LTS
* Debian 7 (development only)
* OSX 10.8+ (development only)


##Known Issues

* AAE must be turned off.
* Riak Search is not supported.
* Multi-Datacenter Replication is not supported.
* Arithmetic operations and aggregates cannot currently be combined.



#Riak TS 1.0.0 Release Notes

Released December 15, 2015.

This release is the introductory release of Riak TS. 

##Contents

1. Features
2. Compatibility
3. Installing
4. Known Issues

##Features
###Customize Tables
Riak TS enables you to define and configure tables of time series data as a riak bucket type, and write data to these tables. The schema of Riak TS's tables are generated as bucket properties and installed as bucket types, which allows you to structure data as it is coming in and store both structured and semi-structured data.

###Data Locality
The structure of Riak TS tables enable data locality based on composite key (including time quanta), which allows for rapid querying and near-linear scaling.

###SQL-like Queries
You can query your data in Riak TS using a subset of SQL.

###Single Key DELETEs and GETs
Riak TS enables single-key DELETs and GETs, which allow you to read and modify data without writing SQL.

###List Key
The list key feature allows you to issue an API call to list all of the keys in your Riak TS database. This can be a useful operation, but it is incredibly resource intensive as all keys must be read and processed. 

###Java, Python, and Erlang Clients
Riak TS offers protobuf requests through three different client APIs: Erlang, Java, and Python.

##Compatibility
Riak TS is compatible with the following operating systems:

* RHEL/CentOS 6
* RHEL/CentOS 7
* Ubuntu 12.04 LTS
* Ubuntu 14.04 LTS
* Debian 6
* Debian 7
* OSX 10.8 (development only)

##Known Issues

* AAE must be turned off.
* Riak Search is not supported.
* Multi-Datacenter Replication is not supported.
* When deleting, a PUT occurs to write the tombstone, then a GET reaps the tombstone. Since PUT and GET are asynchronous, it is possible for the GET to occur before the PUT resulting in the data not actually being deleted. 



#Riak TS 0.9 (Beta) Release Notes

Released November 24, 2015.

Riak TS is a distributed NoSQL database architected to aggregate and analyze massive amounts of sequenced time series data. The Beta release is intended to test the ability to write and read time series data in Riak TS at an appropriate performance level, and better understand customer needs so we can improve future versions of the product.

##Functionality
Features included in 0.9:

* Defining and configuring time series bucket type, including:
  * Data locality based on composite key (including time quantum)
  * Generate schema as bucket properties and install in a bucket type
* Writing data to the time series buckets
* Query TS data using a subset of SQL
* Near-linear scaling
* Ability to store structured and semi-structured data
* Single key gets and deletes
* Clients: Java, Python, Ruby, and Erlang 

Features not supported in Beta:

* AAE
* Time series data in Riak Search or SOLR
* MapReduce support for time series data
* Support for Basho Data Platform
* Performance numbers and tuning will be available with the GA of TS 1.0 only
* MDC support
* Running KV and TS workloads in the same cluster
* Upgrades from existing KV clusters to TS clusters


##Known Issues
* When invoking `riak-admin` to create a TS bucket type, do so as the user who
  runs Riak (`riak` if using the Basho packages). Invoking it as another user
  will result in shell script errors related to improper escaping of the JSON.
* For Beta, AAE must be turned off.
* Queries can only range over 1 to 4 quanta
  * If you write too large a range your query will generate too many sub-queries and the query system will refuse to run it.  
    * Example: Assume a default system with a 15min quanta.
    * A query of “time > 1 o’clock and time < 2 o’clock” will be fine because it covers 4 quanta.
    * A query of “time > 1 o’clock and time < 3 o’clock” will fail because it covers more than 4 quanta.
