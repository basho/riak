"""
Copyright 2010 Rusty Klophaus <rusty@basho.com>
Copyright 2010 Justin Sheehy <justin@basho.com>
Copyright 2009 Jay Baird <jay@mochimedia.com>

This file is provided to you under the Apache License,
Version 2.0 (the "License"); you may not use this file
except in compliance with the License.  You may obtain
a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
"""

# Import libraries.
import sys, random, logging, base64, urllib, re
from cStringIO import StringIO

# Use pycurl as first choice, httplib as second choice.
try:
        import pycurl
        HAS_PYCURL = True
except ImportError:
        import httplib
        HAS_PYCURL = False

# Use json as first choice, simplejson as second choice.
try: 
        import json
except ImportError: 
        import simplejson as json


"""
This file is provided to you under the Apache License,
Version 2.0 (the "License"); you may not use this file
except in compliance with the License.  You may obtain
a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.    
"""

"""
The Riak API for Python allows you to connect to a Riak instance,
create, modify, and delete Riak objects, add and remove links from
Riak objects, run Javascript (and Erlang) based Map/Reduce
operations, and run Linkwalking operations.

See the unit_tests.php file for example usage.

@author Rusty Klophaus (@rklophaus) (rusty@basho.com)
@package RiakAPI
"""

class RiakClient:
        """
        The RiakClient object holds information necessary to connect to
        Riak. The Riak API uses HTTP, so there is no persistent
        connection, and the RiakClient object is extremely lightweight.
        """
        def __init__(self, host='127.0.0.1', port=8098, prefix='riak', mapred_prefix='mapred'):
                """
                Construct a new RiakClient object.
                @param string host - Hostname or IP address (default '127.0.0.1')
                @param int port - Port number (default 8098)
                @param string prefix - Interface prefix (default 'riak')
                @param string mapred_prefix - MapReduce prefix (default 'mapred')
                """
                self._host = host
                self._port = port
                self._prefix = prefix
                self._mapred_prefix = mapred_prefix
                self._client_id = 'php_' + base64.b64encode(str(random.randint(1, 1073741824)))
                self._r = 2
                self._w = 2
                self._dw = 2
                return None

        def get_r(self):
                """
                Get the R-value setting for this ClientObject. (default 2)
                @return integer
                """
                return self._r

        def set_r(self, r):
                """
                Set the R-value for this ClientObject. This value will be used
                for any calls to get(...) or get_binary(...) where where 1) no
                R-value is specified in the method call and 2) no R-value has
                been set in the BucketObject.
                @param integer r - The R value.
                @return self
                """
                self._r = r
                return self

        def get_w(self):
                """
                Get the W-value setting for this ClientObject. (default 2)
                @return integer
                """
                return self._w

        def set_w(self, w):
                """
                Set the W-value for this ClientObject. See set_r(...) for a
                description of how these values are used.
                @param integer w - The W value.
                @return self
                """
                self._w = w
                return self

        def get_dw(self):
                """
                Get the DW-value for this ClientOBject. (default 2)
                @return integer
                """
                return self._dw
        
        def set_dw(self, dw):
                """
                Set the DW-value for this ClientObject. See set_r(...) for a
                description of how these values are used.
                @param integer dw - The DW value.
                @return self
                """
                self._dw = dw
                return self
        
        def get_client_id(self):
                """
                Get the client_id for this ClientObject.
                @return string
                """
                return self._client_id
        
        def set_client_id(self, client_id):
                """
                Set the client_id for this ClientObject. Should not be called
                unless you know what you are doing.
                @param string client_id - The new client_id.
                @return self
                """
                self._client_id = client_id
                return self

        def bucket(self, name):
                """
                Get the bucket by the specified name. Since buckets always exist,
                this will always return a BucketObject.
                @return RiakBucket
                """
                return RiakBucket(self, name)
        
        def is_alive(self):
                """
                Check if the Riak server for this ClientObject is alive.
                @return boolean
                """
                response = RiakUtils.http_request('GET', self._host, self._port, '/ping')
                return(response != None) and (response[1] == 'OK')
        
        def add(self, *args):
                """
                Start assembling a Map/Reduce operation.
                @see RiakMapReduce.add()
                @return RiakMapReduce
                """
                mr = RiakMapReduce(self)
                return apply(mr.add, args)
        
        def link(self, args):
                """
                Start assembling a Map/Reduce operation.
                @see RiakMapReduce.link()
                """
                mr = RiakMapReduce(self)
                return apply(mr.link, args)
        
        def map(self, *args):
                """
                Start assembling a Map/Reduce operation.
                @see RiakMapReduce.map()
                """
                mr = RiakMapReduce(self)
                return apply(mr.map, args)
        
        def reduce(self, *args):
                """
                Start assembling a Map/Reduce operation.
                @see RiakMapReduce.reduce()
                """
                mr = RiakMapReduce(self)
                return apply(mr.reduce, args)
        
class RiakMapReduce:
        """
        The RiakMapReduce object allows you to build up and run a
        map/reduce operation on Riak.
        @package RiakMapReduce
        """

        def __init__(self, client):
                """
                Construct a Map/Reduce object.
                @param RiakClient client - A RiakClient object.
                @return RiakMapReduce
                """
                self._client = client
                self._phases = []
                self._inputs = []
                self._input_mode = None
        
        def add(self, arg1, arg2=None, arg3=None):
                """
                Add inputs to a map/reduce operation. This method takes three
                different forms, depending on the provided inputs. You can
                specify either a RiakObject, a string bucket name, or a bucket,
                key, and additional arg.
                @param mixed arg1 - RiakObject or Bucket
                @param mixed arg2 - Key or blank
                @param mixed arg3 - Arg or blank
                @return RiakMapReduce
                """
                if (arg2 == None) and (arg3 == None):
                        if isinstance(arg1, RiakObject):
                                return self.add_object(arg1)
                        else:
                                return self.add_bucket(arg1)
                else:
                        return self.add_bucket_key_data(arg1, arg2, arg3)
                
        def add_object(self, obj):
                return self.add_bucket_key_data(obj._bucket._name, obj._key, None)
                
        def add_bucket_key_data(self, bucket, key, data) :
                if self._input_mode == 'bucket':
                        raise Exception('Already added a bucket, can\'t add an object.')
                else:
                        self._inputs.append([bucket, key, data])
                        return self

        def add_bucket(self, bucket) :
                self._input_mode = 'bucket'
                self._inputs = bucket
                return self

        def link(self, bucket='_', tag='_', keep=False):
                """
                Add a link phase to the map/reduce operation.
                @param string bucket - Bucket name (default '_', which means all
                buckets)
                @param string tag - Tag (default '_', which means all buckets)
                @param boolean keep - Flag whether to keep results from this
                stage in the map/reduce. (default False, unless this is the last
                step in the phase)
                @return self
                """
                self._phases.append(RiakLinkPhase(bucket, tag, keep))
                return self
        
        def map(self, function, options=[]):
                """
                Add a map phase to the map/reduce operation.
                @param mixed function - Either a named Javascript function (ie:
                'Riak.mapValues'), or an anonymous javascript function (ie:
                'function(...)  ... ' or an array ['erlang_module',
                'function'].
                @param array() options - An optional associative array
                containing 'language', 'keep' flag, and/or 'arg'.
                @return self
                """
                if isinstance(function, list):
                        language = 'erlang'
                else: 
                        language='javascript'

                mr = RiakMapReducePhase('map',
                                        function,
                                        RiakUtils.get_value('language', options, language),
                                        RiakUtils.get_value('keep', options, False),
                                        RiakUtils.get_value('arg', options, None))
                self._phases.append(mr)
                return self

        def reduce(self, function, options=[]):
                """
                Add a reduce phase to the map/reduce operation.
                @param mixed function - Either a named Javascript function (ie.
                'Riak.mapValues'), or an anonymous javascript function(ie:
                'function(...) { ... }' or an array ['erlang_module', 'function'].
                @param array() options - An optional associative array
                containing 'language', 'keep' flag, and/or 'arg'.
                @return self
                """

                if isinstance(function, list):
                        language = 'erlang'
                else: 
                        language='javascript'

                mr = RiakMapReducePhase('reduce',
                                        function,
                                        RiakUtils.get_value('language', options, language),
                                        RiakUtils.get_value('keep', options, False),
                                        RiakUtils.get_value('arg', options, None))
                self._phases.append(mr)
                return self
                        
        def run(self, timeout=None):
                """
                Run the map/reduce operation. Returns an array of results, or an
                array of RiakLink objects if the last phase is a link phase.
                @param integer timeout - Timeout in seconds.
                @return array()
                """
                # Convert all phases to associative arrays. Also,
                # if none of the phases are accumulating, then set the last one to
                # accumulate.
                num_phases = len(self._phases)
                keep_flag = False
                query = []
                for i in range(num_phases):
                        phase = self._phases[i]
                        if (i == (num_phases - 1)) and (not keep_flag):
                                phase._keep = True
                        if phase._keep: keep_flag = True
                        query.append(phase.to_array())

                # Construct the job, optionally set the timeout...
                job = {'inputs':self._inputs, 'query':query}
                if timeout != None:
                        job['timeout'] = timeout

                content = json.dumps(job)

                # Do the request...
                host = self._client._host
                port = self._client._port
                url = "/" + self._client._mapred_prefix
                response = RiakUtils.http_request('POST', host, port, url, {}, content)
                result = json.loads(response[1])
                
                # If the last phase is NOT a link phase, then return the result.
                lastIsLink = isinstance(self._phases[-1], RiakLinkPhase)
                if not lastIsLink:
                        return result

                # Otherwise, if the last phase IS a link phase, then convert the
                # results to RiakLink objects.
                a = []
                for r in result:
                        link = RiakLink(r[0], r[1], r[2])
                        link._client = self._client
                        a.append(link)

                return a

class RiakMapReducePhase:
        """
        The RiakMapReducePhase holds information about a Map phase or
        Reduce phase in a RiakMapReduce operation.
        """

        def __init__(self, type, function, language, keep, arg):
                """
                Construct a RiakMapReducePhase object.
                @param string type - 'map'placeholder149'reduce'
                @param mixed function - string or array():
                @param string language - 'javascript'placeholder149'erlang'
                @param boolean keep - True to return the output of this phase in
                the results.
                @param mixed arg - Additional value to pass into the map or
                reduce function.
                """
                self._type = type
                self._language = language
                self._function = function
                self._keep = keep
                self._arg = arg
                return None

        def to_array(self):
                """
                Convert the RiakMapReducePhase to an associative array. Used
                internally.
                """
                stepdef = {'keep':self._keep,
                           'language':self._language,
                           'arg':self._arg}
                        
                if (self._language == 'javascript') and isinstance(self._function, list):
                        stepdef['bucket'] = self._function[0]
                        stepdef['key'] = self._function[1]
                elif (self._language == 'javascript') and isinstance(self._function, str):
                        if ("{" in self._function):
                                stepdef['source'] = self._function
                        else:
                                stepdef['name'] = self._function

                elif (self._language == 'erlang' and isinstance(self._function, list)):
                        stepdef['module'] = self._function[0]
                        stepdef['function'] = self._function[1]

                return {self._type : stepdef}

class RiakLinkPhase :
        """
        The RiakLinkPhase object holds information about a Link phase in a
        map/reduce operation.
        @package RiakLinkPhase
        """

        def __init__(self, bucket, tag, keep):
                """
                Construct a RiakLinkPhase object.
                @param string bucket - The bucket name.
                @param string tag - The tag.
                @param boolean keep - True to return results of this phase.
                """
                self._bucket = bucket
                self._tag = tag
                self._keep = keep
                return None

        def to_array(self):
                """
                Convert the RiakLinkPhase to an associative array. Used
                internally.
                """
                stepdef = {'bucket':self._bucket,
                           'tag':self._tag,
                           'keep':self._keep}
                return {'link':stepdef}

class RiakLink :
        """
        The RiakLink object represents a link from one Riak object to
        another.
        @package RiakLink
        """

        def __init__(self, bucket, key, tag=None):
                """
                Construct a RiakLink object.
                @param string bucket - The bucket name.
                @param string key - The key.
                @param string tag - The tag.
                """
                self._bucket = bucket
                self._key = key
                self._tag = tag
                self._client = None
                return None

        def get(self, r=None):
                """
                Retrieve the RiakObject to which this link points.
                @param integer r - The R-value to use.
                @return RiakObject
                """
                return self._client._bucket(self._bucket).get(self._key, r)

        def get_binary(self, r=None):
                """
                Retrieve the RiakObject to which this link points, as a binary.
                @param integer r - The R-value to use.
                @return RiakObject
                """
                return self._client._bucket(self._bucket).get_binary(self._key, r)

        def get_bucket(self):
                """
                Get the bucket name of this link.
                @return string
                """
                return self._bucket

        def set_bucket(self, name):
                """
                Set the bucket name of this link.
                @param string name - The bucket name.
                @return self
                """
                self._bucket = bucket
                return self

        def get_key(self):
                """
                Get the key of this link.
                @return string
                """
                return self._key
        
        def set_key(self, key):
                """
                Set the key of this link.
                @param string key - The key.
                @return self
                """
                self._key = key
                return self

        def get_tag(self):
                """
                Get the tag of this link.
                @return string
                """
                if (self._tag == None):
                        return self._bucket
                else:
                        return self._tag

        def set_tag(self, tag):
                """
                Set the tag of this link.
                @param string tag - The tag.
                @return self
                """
                self._tag = tag
                return self

        def to_link_header(self, client):
                """
                Convert this RiakLink object to a link header string. Used internally.
                """
                link = ''
                link += '</'
                link += client._prefix + '/'
                link += urllib.quote_plus(self._bucket) + '/'
                link += urllib.quote_plus(self._key) + '>; riaktag="'
                link += urllib.quote_plus(self.get_tag()) + '"'
                return link

        def isEqual(self, link):
                """
                Return True if the links are equal.
                @param RiakLink link - A RiakLink object.
                @return boolean
                """
                is_equal = (self._bucket == link._bucket) and (self._key == link._key) and (self.get_tag() == link.get_tag())
                return is_equal

class RiakBucket :
        """
        The RiakBucket object allows you to access and change information
        about a Riak bucket, and provides methods to create or retrieve
        objects within the bucket.
        @package RiakBucket
        """

        def __init__(self, client, name):
                self._client = client
                self._name = name
                self._r = None
                self._w = None
                self._dw = None
                return None

        def get_r(self, r=None):
                """
                Get the R-value for this bucket, if it is set, otherwise return
                the R-value for the client.
                @return integer
                """
                if (r != None):
                        return r
                if (self._r != None):
                        return self._r
                return self._client.get_r()

        def set_r(self, r):
                """
                Set the R-value for this bucket. get(...) and get_binary(...)
                operations that do not specify an R-value will use this value.
                @param integer r - The new R-value.
                @return self
                """
                self._r = r
                return self
                                                         
        def get_w(self, w):
                """
                Get the W-value for this bucket, if it is set, otherwise return
                the W-value for the client.
                @return integer
                """
                if (w != None):
                        return w
                if (self._w != None):
                        return self._w
                return self._client.get_w()
        
        def set_w(self, w):
                """
                Set the W-value for this bucket. See set_r(...) for more information.
                @param integer w - The new W-value.
                @return self
                """
                self._w = w
                return self

        def get_dw(self, dw):
                """
                Get the DW-value for this bucket, if it is set, otherwise return
                the DW-value for the client.
                @return integer
                """
                if (dw != None):
                        return dw
                if (self._dw != None):
                        return self._dw
                return self._client.get_dw()
                                                         
        def set_dw(self, dw):
                """
                Set the DW-value for this bucket. See set_r(...) for more information.
                @param integer dw - The new DW-value
                @return self
                """
                self._dw = dw
                return self
                                                         
        def new(self, key, data=None):
                """
                Create a new Riak object that will be stored as JSON.
                @param string key - Name of the key.
                @param object data - The data to store. (default None)
                @return RiakObject
                """
                obj = RiakObject(self._client, self, key)
                obj.set_data(data)
                obj.set_content_type('text/json')
                obj._jsonize = True
                return obj
        
        def new_binary(self, key, data, content_type='text/json'):
                """
                Create a new Riak object that will be stored as plain text/binary.
                @param string key - Name of the key.
                @param object data - The data to store.
                @param string content_type - The content type of the object. (default 'text/json')
                @return RiakObject
                """
                obj = RiakObject(self._client, self, key)
                obj.set_data(data)
                obj.set_content_type('text/json')
                obj._jsonize = False
                return obj

        def get(self, key, r=None):
                """
                Retrieve a JSON-encoded object from Riak.
                @param string key - Name of the key.
                @param int r - R-Value of the request (defaults to bucket's R)
                @return RiakObject
                """
                obj = RiakObject(self._client, self, key)
                obj._jsonize = True
                r = self.get_r(r)
                return obj.reload(r)

        def get_binary(self, key, r=None):
                """
                Retrieve a binary/string object from Riak.
                @param string key - Name of the key.
                @param int r - R-Value of the request (defaults to bucket's R)
                @return RiakObject
                """
                obj = RiakObject(self._client, self, key)
                obj._jsonize = False
                r = self.get_r(r)
                return obj.reload(r)

        def set_n_val(self, nval):
                """
                Set the N-value for this bucket, which is the number of replicas
                that will be written of each object in the bucket. Set this once
                before you write any data to the bucket, and never change it
                again, otherwise unpredictable things could happen. This should
                only be used if you know what you are doing.
                @param integer nval - The new N-Val.
                """
                return self.set_property('n_val', nval)

        def get_n_val(self):
                """
                Retrieve the N-value for this bucket.
                @return integer                                                         
                """
                return self.get_property('n_val')

        def set_allow_multiples(self, bool):
                """
                If set to True, then writes with conflicting data will be stored
                and returned to the client. This situation can be detected by
                calling has_siblings() and get_siblings(). This should only be used
                if you know what you are doing.
                @param boolean bool - True to store and return conflicting writes.                                                         
                """
                return self.set_property('allow_mult', bool)

        def get_allow_multiples(self):
                """
                Retrieve the 'allow multiples' setting.
                @return Boolean
                """
                return self.get_property('allow_mult') == True

        def set_property(self, key, value):
                """
                Set a bucket property. This should only be used if you know what
                you are doing.
                @param string key - Property to set.
                @param mixed value - Property value.                                                         
                """
                return self.set_properties({key : value})

        def get_property(self, key):
                """
                Retrieve a bucket property.
                @param string key - The property to retrieve.
                @return mixed
                """
                props = self.get_properties()
                if (key in props.keys()):
                        return props[key]
                else:
                        return None

        def set_properties(self, props):
                """
                Set multiple bucket properties in one call. This should only be
                used if you know what you are doing.
                @param array props - An associative array of key:value.        
                """

                #Construct the URL, Headers, and Content...
                host, port, url = RiakUtils.build_rest_path(self._client, self)
                headers = {'Content-Type' : 'application/json'}
                content = json.dumps({'props' : props})
	
                #Run the request...
                response = RiakUtils.http_request('PUT', host, port, url, headers, content)

                # Handle the response...
                if (response == None):
                        raise Exception('Error setting bucket properties.')
        
                # Check the response value...
                status = response[0]['http_code']
                if (status != 204):
                        raise Exception('Error setting bucket properties.')

        def get_properties(self):
                """
                Retrieve an associative array of all bucket properties.
                @return Array		
                """
                
                # Run the request...
                params = {'props' : 'True', 'keys' : 'False'}
                host, port, url = RiakUtils.build_rest_path(self._client, self, None, None, params)
                response = RiakUtils.http_request('GET', host, port, url)
                
                # Use a RiakObject to interpret the response, we are just interested in the value.
                obj = RiakObject(self._client, self, None)
                obj.populate(response, [200])
                if (not obj.exists()):
                        raise Exception('Error getting bucket properties.')

                props = obj.get_data()
                props = props['props']
                return props

class RiakObject :
        """
        The RiakObject holds meta information about a Riak object, plus the
        object's data.
        @package RiakObject	
        """

        def __init__(self, client, bucket, key=None):
                """
                Construct a new RiakObject.
                @param RiakClient client - A RiakClient object.
                @param RiakBucket bucket - A RiakBucket object.
                @param string key - An optional key. If not specified, then key
                is generated by server when store(...) is called.		
                """
                self._client = client
                self._bucket = bucket
                self._key = key
                self._jsonize = True
                self._headers = {}
                self._links = []
                self._siblings = []
                self._exists = False
                return None

        def get_data(self):
                """
                Get the data stored in this object. Will return a associative
                array, unless the object was constructed with new_binary(...) or
                get_binary(...), in which case this will return a string.
                @return array or string		
                """
                return self._data
        
        def set_data(self, data):
                """
                Set the data stored in this object. This data will be
                JSON encoded unless the object was constructed with
                new_binary(...) or get_binary(...).
                @param mixed data - The data to store.
                @return data		
                """
                self._data = data
                return self
                
        def status(self):
                """
                Get the HTTP status from the last operation on this object.
                @return integer
                """
                return self._headers['http_code']
        
        def exists(self):
                """
                Return True if the object exists, False otherwise. Allows you to
                detect a get(...) or get_binary(...) operation where the object is missing.
                @return boolean
                """
                return self._exists
        
        def get_content_type(self):
                """
                Get the content type of this object. This is either text/json, or
                the provided content type if the object was created via new_binary(...).
                @return string
                """
                return self._headers['content-type']
        
        def set_content_type(self, content_type):
                """
                Set the content type of this object.
                @param string content_type - The new content type.
                @return self		
                """
                self._headers['content-type'] = content_type
                return self
        
        def add_link(self, obj, tag=None):
                """
                Add a link to a RiakObject.
                @param mixed obj - Either a RiakObject or a RiakLink object.
                @param string tag - Optional link tag. (default is bucket name,
                ignored if obj is a RiakLink object.)
                @return RiakObject		
                """
                if isinstance(obj, RiakLink):
                        newlink = obj
                else:
                        newlink = RiakLink(obj._bucket._name, obj._key, tag)
                        
                self.remove_link(newlink)
                self._links.append(newlink)
                return self
                
        def remove_link(self, obj, tag=None):
                """
                Remove a link to a RiakObject.
                @param mixed obj - Either a RiakObject or a RiakLink object.
                @param string tag -
                @param mixed obj - Either a RiakObject or a RiakLink object.
                @param string tag - Optional link tag. (default is bucket name,
                ignored if obj is a RiakLink object.)
                @return self		
                """
                if isinstance(obj, RiakLink):
                        oldlink = obj
                else:
                        oldlink = RiakLink(obj._bucket._name, obj._key, tag)
                        
                a = []
                for link in self._links:
                        if not link.isEqual(oldlink):
                                a.append(link)

                self._links = a
                return self

        def get_links(self):
                """
                Return an array of RiakLink objects.
                @return array()		
                """
                # Set the clients before returning...
                for link in self._links:
                        link._client = self._client
                return self._links
                        
        def store(self, w=None, dw=None):
                """
                Store the object in Riak. When this operation completes, the
                object could contain new metadata and possibly new data if Riak
                contains a newer version of the object according to the object's
                vector clock.
                @param integer w - W-value, wait for this many partitions to respond
                before returning to client.
                @param integer dw - DW-value, wait for this many partitions to
                confirm the write before returning to client.
                @return self		
                """
                # Use defaults if not specified...
                w = self._bucket.get_w(w)
                dw = self._bucket.get_dw(w)
                
                # Construct the URL...
                params = {'returnbody' : 'true', 'w' : w, 'dw' : dw}
                host, port, url = RiakUtils.build_rest_path(self._client, self._bucket, self._key, None, params)
                
                # Construct the headers...
                headers = {'Accept' : 'text/plain, */*; q=0.5',
                           'Content-Type' : self.get_content_type(),
                           'X-Riak-ClientId' : self._client.get_client_id()}
                
                # Add the vclock if it exists...
                if (self.vclock() != None):
                        headers['X-Riak-Vclock'] = self.vclock()
                        
                # Add the Links...
                headers['Link'] = ''
                for link in self._links:
                        if headers['Link'] != '': headers['Link'] += ', '
                        headers['Link'] += link.to_link_header(self._client)
                        
                if (self._jsonize):
                        content = json.dumps(self.get_data())
                else:
                        content = self.get_data()
                
                # Run the operation.
                response = RiakUtils.http_request('PUT', host, port, url, headers, content)
                self.populate(response, [200, 300])
                return self

        def reload(self, r=None):
                """
                Reload the object from Riak. When this operation completes, the
                object could contain new metadata and a new value, if the object
                was updated in Riak since it was last retrieved.
                @param integer r - R-Value, wait for this many partitions to respond
                before returning to client.
                @return self		
                """
                # Do the request...
                r = self._bucket.get_r(r)
                params = {'r' : r}
                host, port, url = RiakUtils.build_rest_path(self._client, self._bucket, self._key, None, params)
                response = RiakUtils.http_request('GET', host, port, url)
                self.populate(response, [200, 300, 404])
                                
                # If there are siblings, load the data for the first one by default...
                if (self.has_siblings()):
                        obj = self.get_sibling(0)
                        self.set_data(obj.get_data())
                        
                return self

        def delete(self, dw=None):
                """
                Delete this object from Riak.
                @param integer dw - DW-value. Wait until this many partitions have
                deleted the object before responding.
                @return self		
                """
                # Use defaults if not specified...
                dw = self._bucket.get_dw(dw)
                
                # Construct the URL...
                params = {'dw' : dw}
                host, port, url = RiakUtils.build_rest_path(self._client, self._bucket, self._key, None, params)
                
                # Run the operation...
                response = RiakUtils.http_request('DELETE', host, port, url)
                self.populate(response, [204, 404])
                return self
                        
        def clear(self) :
                """
                Reset this object.
                @return self		
                """
                self._headers = []
                self._links = []
                self._data = None
                self._exists = False
                self._siblings = []
                return self
        
        def vclock(self) :
                """
                Get the vclock of this object.
                @return string		
                """
                if ('x-riak-vclock' in self._headers.keys()):
                        return self._headers['x-riak-vclock']
                else:
                        return None

        def populate(self, response, expected_statuses):
                """
                Given the output of RiakUtils.http_request and a list of
                statuses, populate the object. Only for use by the Riak client
                library.
                @return self		
                """
                self.clear()
                                
                # If no response given, then return.
                if (response == None):
                        return self
                
                # Update the object...
                self._headers = response[0]
                self._data = response[1]
                status = self.status()
                                
                # Check if the server is down(status==0)
                if (status == 0):
                        m = 'Could not contact Riak Server: http://' + self._client._host + ':' + str(self._client._port) + '!'
                        raise Exception(m)

                # Verify that we got one of the expected statuses. Otherwise, raise an exception.
                if (not status in expected_statuses):
                        m = 'Expected status ' + str(expected_statuses) + ', received ' + str(status)
                        raise Exception(m)

                # If 404(Not Found), then clear the object.
                if (status == 404):
                        self.clear()
                        return self
                        
                # If we are here, then the object exists...
                self._exists = True
	
                # Parse the link header...
                if ('link' in self._headers.keys()):
                        self.populate_links(self._headers['link'])

                # If 300(Siblings), then load the first sibling, and
                # store the rest.
                if (status == 300):
                        siblings = self._data.strip().split('\n')
                        siblings.pop(0)

                        # Get rid of 'Siblings:' string.
                        self._siblings = siblings
                        self._exists = True
                        return self

                # Possibly json_decode...
                if (status == 200 and self._jsonize):
                        self._data = json.loads(self._data)
        
                return self

        def populate_links(self, linkHeaders) :
                """
                Private.
                @return self		
                """
                for linkHeader in linkHeaders.strip().split(','):
                        linkHeader = linkHeader.strip()
                        matches = re.match("\<\/([^\/]+)\/([^\/]+)\/([^\/]+)\>; ?riaktag=\"([^\']+)\"", linkHeader)
                        if (matches != None):
                                link = RiakLink(matches.group(2), matches.group(3), matches.group(4))
                                self._links.append(link)
                return self

        def has_siblings(self):
                """
                Return True if this object has siblings.
                @return boolean
                """
                return(self.get_sibling_count() > 0)
        
        def get_sibling_count(self):
                """
                Get the number of siblings that this object contains.
                @return integer		
                """
                return len(self._siblings)
        
        def get_sibling(self, i, r=None):
                """
                Retrieve a sibling by sibling number.
                @param  integer i - Sibling number.
                @param  integer r - R-Value. Wait until this many partitions
                have responded before returning to client.
                @return RiakObject.		
                """
                # Use defaults if not specified.
                r = self._bucket.get_r(r)
                
                # Run the request...
                vtag = self._siblings[i]
                params = {'r' : r, 'vtag' : vtag}
                host, port, url = RiakUtils.build_rest_path(self._client, self._bucket, self._key, None, params)
                response = RiakUtils.http_request('GET', host, port, url)
                
                # Respond with a new object...
                obj = RiakObject(self._client, self._bucket, self._key)
                obj._jsonize = self._jsonize
                obj.populate(response, [200])
                return obj

        def get_siblings(self, r=None):
                """
                Retrieve an array of siblings.
                @param integer r - R-Value. Wait until this many partitions have
                responded before returning to client.
                @return array of RiakObject		
                """
                a = []
                for i in range(self.get_sibling_count()):
                        a.append(self.get_sibling(i, r))
                return a
                
        def add(self, *args):
                """
                Start assembling a Map/Reduce operation.
                @see RiakMapReduce.add()
                @return RiakMapReduce		
                """
                mr = RiakMapReduce(self._client)
                mr.add(self._bucket._name, self._key)
                return apply(mr.add, args)
        
        def link(self, *args):
                """
                Start assembling a Map/Reduce operation.
                @see RiakMapReduce.link()
                @return RiakMapReduce		
                """
                mr = RiakMapReduce(self._client)
                mr.add(self._bucket._name, self._key)
                return apply(mr.link, args)
        
        def map(self, *args):
                """
                Start assembling a Map/Reduce operation.
                @see RiakMapReduce.map()
                @return RiakMapReduce
                """
                mr = RiakMapReduce(self._client)
                mr.add(self._bucket._name, self._key)
                return apply(mr.map, args)
        
        def reduce(self, params):
                """
                Start assembling a Map/Reduce operation.
                @see RiakMapReduce.reduce()
                @return RiakMapReduce
                """
                mr = RiakMapReduce(self._client)
                mr.add(self._bucket._name, self._key)
                return apply(mr.reduce, args)
        
class RiakUtils :
        """
        Utility functions used by Riak library.
        @package RiakUtils	
        """
        @classmethod
        def get_value(self, key, array, defaultValue) :
                if (key in array):
                        return array[key]
                else:
                        return defaultValue
		
        @classmethod
        def build_rest_path(self, client, bucket, key=None, spec=None, params=None) :
                """
                Given a ClientObject, BucketObject, Key, LinkSpec, and Params,
                construct and return a URL.		
                """
                # Build 'http://hostname:port/prefix/bucket'
                path = ''
		path += '/' + client._prefix
		path += '/' + urllib.quote_plus(bucket._name)

                # Add '.../key'
                if (key != None):
                        path += '/' + urllib.quote_plus(key)
                        
                # Add query parameters.
                if (params != None):
                        s = ''
                        for key in params.keys():
                                if (s != ''): s += '&'
                                s += urllib.quote_plus(key) + '=' + urllib.quote_plus(str(params[key]))
                        path += '?' + s

                # Return.
                return client._host, client._port, path

        @classmethod
        def http_request(self, method, host, port, url, headers = {}, obj = '') :
                """
                Given a Method, URL, Headers, and Body, perform and HTTP request,
                and return an array of arity 2 containing an associative array of
                response headers and the response body.
                """
                if HAS_PYCURL:
                        return self.pycurl_request(method, host, port, url, headers, obj)
                else:
                        return self.httplib_request(method, host, port, url, headers, obj)


        @classmethod
        def httplib_request(self, method, host, port, uri, headers={}, body=''):
                # Run the request...
                client = None
                response = None
                try:
                        client = httplib.HTTPConnection(host, port)
                        client.request(method, uri, body, headers)
                        response = client.getresponse()

                        # Get the response headers...
                        response_headers = {}
                        response_headers['http_code'] = response.status
                        for (key, value) in response.getheaders():
                                response_headers[key.lower()] = value

                        # Get the body...
                        response_body = response.read()
                        response.close()

                        return response_headers, response_body
                except:
                        if client != None: client.close()
                        if response != None: response.close()
                        raise

        
        @classmethod
        def pycurl_request(self, method, host, port, uri, headers={}, body=''):
                url = "http://" + host + ":" + str(port) + uri
                # Set up Curl...
                client = pycurl.Curl()
                client.setopt(pycurl.URL, url)                
                client.setopt(pycurl.HTTPHEADER, self.build_headers(headers))
                if method == 'GET':
                        client.setopt(pycurl.HTTPGET, 1)
                elif method == 'POST':
                        client.setopt(pycurl.POST, 1)                        
                        client.setopt(pycurl.POSTFIELDS, body)
                elif method == 'PUT':
                        client.setopt(pycurl.CUSTOMREQUEST, method)        
                        client.setopt(pycurl.POSTFIELDS, body)
                elif method == 'DELETE':
                        client.setopt(pycurl.CUSTOMREQUEST, method)

                # Capture the response headers...
                response_headers_io = StringIO()
                client.setopt(pycurl.HEADERFUNCTION, response_headers_io.write)
                
                # Capture the response body...
                response_body_io = StringIO()
                client.setopt(pycurl.WRITEFUNCTION, response_body_io.write)

                try:
                        # Run the request.
                        client.perform()
                        http_code = client.getinfo(pycurl.HTTP_CODE)
                        client.close()
                        
                        # Get the headers...
                        response_headers = self.parse_http_headers(response_headers_io.getvalue())
                        response_headers['http_code'] = http_code
                        
                        # Get the body...
                        response_body = response_body_io.getvalue()

                        return response_headers, response_body
                except:
                        if (client != None) : client.close()
                        raise

        @classmethod
        def build_headers(self, headers):
                headers1 = []
                for key in headers.keys():
                        headers1.append('%s: %s' % (key, headers[key]))
                return headers1

        @classmethod
        def parse_http_headers(self, headers) :
                """
                Parse an HTTP Header string into an asssociative array of
                response headers.		
                """
                retVal = {}
                fields = headers.split("\n")
                for field in fields:
                        matches = re.match("([^:]+):(.+)", field)
                        if (matches == None): continue
                        key = matches.group(1).lower()
                        value = matches.group(2).strip()
                        if (key in retVal.keys()):
                                if  isinstance(retVal[key], list):
                                        retVal[key].append(value)
                                else:
                                        retVal[key] = [retVal[key]].append(value)
                        else:
                                retVal[key] = value
                return retVal
                        
