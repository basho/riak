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
