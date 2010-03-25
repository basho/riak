ripple
======

ripple is a rich Ruby client for Riak, Basho's distributed database. It includes
two namespaces:

* "Riak" contains a basic wrapper around typical operations, including bucket
   manipulation, object CRUD, link-walking, and map-reduce.
* "Ripple" contains an ActiveModel-compatible modeling layer that is inspired by
   ActiveRecord, DataMapper, and MongoMapper.

------------
Installation
------------

It is easiest to install Ripple using Rubygems.  Install the activemodel and 
activesupport beta gems first, using "sudo" as necessary. If you have Rubygems 
1.3.6 or later, only the last line is necessary.

$ gem install tzinfo builder memcache-client i18n
$ gem install activemodel activesupport --pre
$ gem install ripple

------------
Dependencies
------------

ripple requires Ruby 1.8.7 or later and versions 3 or above of ActiveModel and 
ActiveSupport (and their dependencies, including i18n).  I highly recommend the 
"curb" (http://curb.rubyforge.org/) gem for better HTTP client performance.

In development, you will also need these gems:

* jeweler
* rspec >= 1.3
* fakeweb >= 1.2
* curb >= 0.6
* rack >= 1.0
* yard >= 0.5.2

-------------
Basic Example
-------------

require 'riak'

# Create a client interface
client = Riak::Client.new

# Retrieve a bucket
bucket = client.bucket("doc")  # a Riak::Bucket

# Get an object from the bucket
object = bucket.get("index.html")   # a Riak::RObject

# Change the object's data and save
object.data = "<html><body>Hello, world!</body></html>"
object.store

# Reload an object you already have
object.reload                  # Works if you have the key and vclock, using 
                               # conditional GET
object.reload :force => true   # Reloads whether you have the vclock or not

# Access more like a hash, client[bucket][key]
client['doc']['index.html']   # the Riak::RObject

# Create a new object
new_one = Riak::RObject.new(bucket, "application.js")
new_one.content_type = "application/javascript" # You must set the content type.
new_one.data = "alert('Hello, World!')"
new_one.store

------------------
Map-Reduce Example
------------------

# Assuming you've already instantiated a client, get the album titles for 
# The Beatles
results = Riak::MapReduce.new(client).
                add("artists","Beatles").
                link(:bucket => "albums").
                map("function(v){ return [JSON.parse(v.values[0].data).title]; }", 
                    :keep => true).run

p results # => ["Please Please Me", "With The Beatles", "A Hard Day's Night", 
          #     "Beatles For Sale", "Help!", "Rubber Soul",
          #     "Revolver", "Sgt. Pepper's Lonely Hearts Club Band", 
          #     "Magical Mystery Tour", "The Beatles", "Yellow Submarine", 
          #     "Abbey Road", "Let It Be"]

----------------------
Document model Example
----------------------

require 'ripple'

class Email
  include Ripple::Document
  property :from,    String, :presence => true
  property :to,      String, :presence => true
  property :sent,    Time,   :default => proc { Time.now }
  property :body,    String
end

email = Email.find("37458abc752f8413e")  # GET /riak/emails/37458abc752f8413e
email.from = "someone@nowhere.net"
email.save                               # PUT /riak/emails/37458abc752f8413e

reply = Email.new
reply.from = "justin@bashoooo.com"
reply.to   = "sean@geeemail.com"
reply.body = "Riak is a good fit for scalable Ruby apps."
reply.save                               # POST /riak/emails (Riak-assigned key)


-----------------
How to Contribute
-----------------

* Fork the project on Github (http://github.com/seancribbs/ripple).  If you have
  already forked, use "git pull --rebase" to reapply your changes on top of the 
  mainline. Example:
  $ git checkout master
  $ git pull --rebase seancribbs master

* Create a topic branch. If you've already created a topic branch, rebase it on 
  top of changes from the mainline "master" branch. Examples:
  New branch:
    $ git checkout -b topic
  Existing branch:
    $ git rebase master

* Write an RSpec example, set of examples, and/or Cucumber story that demonstrate
  the necessity and validity of your changes. *Patches without specs will most 
  often be ignored. Just do it, you'll thank me later.* Documentation patches 
  need no specs, of course.

* Make your feature addition or bug fix. Make your specs and stories pass.

* Run the suite using multiruby or rvm to ensure cross-version compatibility.

* Cleanup any trailing whitespace in your code (try "whitespace-mode" in Emacs, 
  or "Remove Trailing Spaces in Document" in the "Text" bundle in Textmate).

* Commit, do not mess with Rakefile or VERSION.  If related to an existing issue 
  in the tracker (http://github.com/seancribbs/ripple/issues), include 
  "Closes #X" in the commit message (where X is the issue number).

* Send me a pull request on Github.

-------------------
License & Copyright
-------------------

Copyright 2010 Sean Cribbs, Sonian Inc., and Basho Technologies, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

------------------
Auxillary Licenses
------------------

The included photo (spec/fixtures/cat.jpg) is Copyright 2009 Sean Cribbs, and is
licensed under the Creative Commons Attribution Non-Commercial 3.0 license 
(http://creativecommons.org/licenses/by-nc/3.0).

The "Poor Man's Fibers" implementation (lib/riak/util/fiber1.8.rb) is Copyright 
2008 Aman Gupta.
