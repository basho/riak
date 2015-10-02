
#Riak TS 0.8 (Beta) Release Notes

Released October 5, 2015.

Riak TS is a distributed NoSQL database architected to aggregate and analyze massive amounts of sequenced time series data. The Beta release is intended to test the ability to write and read time series data in Riak TS at an appropriate performance level, and better understand customer needs so we can improve future versions of the product.

##Functionality
Features included in Beta:

* Defining and configuring time series bucket type, including:
  * Data locality based on Composite Key
  * Data locality based on time quantum
  * Bucket schema definition
  * Generate schema as bucket properties and install in a bucket type
* Writing data to the time series buckets
* Query TS data using a subset of SQL
* Near-linear scaling
* Ability to store structured and semi-structured data
* Operating Systems: CentOS 6 & 7, Debian 6 & 7, Ubuntu 14.04
* Clients: Java and Erlang 

Features not supported in Beta:

* AAE
* Time series data in Riak Search or SOLR
* MapReduce support for time series data
* Single key deletes for time series data
* Support for Basho Data Platform
* Performance numbers and tuning will be available with the GA of TS 1.0 only
* MDC support
* Running KV and TS workloads in the same cluster
* Upgrades from existing KV clusters to TS clusters


##Known Issues
* For Beta, AAE must be turned off.
* Queries can only range over 1 to 4 quanta
  * If you write too large a range your query will generate too many sub-queries and the query system will refuse to run it.  
    * Example: Assume a default system with a 15min quanta.
    * A query of “time > 1 o’clock and time < 2 o’clock” will be fine because it covers 4 quanta.
    * A query of “time > 1 o’clock and time < 3 o’clock” will fail because it covers more than 4 quanta.
* If you use monthly quanta, there may be data loss. For now, please do not use month quanta.

##What has been tested
* Riak KV regression Tests
* Table activation (1 node, 3 nodes)
* Table creation failure scenarios (1 node)
* Table creation (1 node, 3 nodes)
* PUT failure scenarios (1 node)
* PUT (1 node, 3 nodes)
* Select failure scenarios (1 node)
* Select (1 node, 3 nodes)
* Bucket creation and activation using riak-admin (3 nodes)
* Data creation and retrieval using the Erlang client (3 nodes)

##What has not been tested
* AAE
* MDC
* Performance Tests
* Stress / Load Tests
* Riak Search (Yokozuna)

