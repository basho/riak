#!/usr/bin/env python
"""
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

import urllib
from cStringIO import StringIO

try:
    import pycurl
    HAS_PYCURL = True
except ImportError:
    import httplib
    HAS_PYCURL = False
    
try:
	import json
except ImportError:
    import simplejson as json

def expect(status):
    """Wraps a function in a function that guarantees a return code(s) and 
       if certain conditions are met either return None or a Dictionary from
       a JSON string.
    
    """
    if not isinstance(status, (list, tuple)):
        status = [status]
    def wrapper_func(f):
        def wrapped_func(*args, **kwargs):
            code, resp = f(*args, **kwargs)
            if HAS_PYCURL:
                resp.reset()
            if code not in status:
                raise JiakException(code, resp.read())
            if code in [404, 204]:
                return None
            else:
                return json.load(resp)
        return wrapped_func
    return wrapper_func

def build_headers(h, disable_continue=True):
    headers = ['Expect:'] if disable_continue else []
    for k,v in h.iteritems():
        headers.append('%s: %s' % (k, v))
    return headers

class JiakException(Exception): pass

class Jiak(object):
    """A Python interface for the Riak (http://riak.basho.com/) key-value store.
       The Riak source does ship with a client library for python, but I wanted
       something more Pythonic and I wanted to use pycURL.

       Example Usage:
       
       >>> client = Jiak('127.0.0.1', 8098, 'jiak')
       >>> [client.delete('jiak_example', key) for key in ['doctestkey', 'jroot', 'jleaf1', 'jleaf2', 'jleaf3']]
       [None, None, None, None, None]
       >>> obj = client.store('jiak_example', 'doctestkey', {'foo':2})
       >>> client.fetch('jiak_example', 'doctestkey').get('object', {}).get('foo') == 2
       True
       
    """
    def __init__(self, host, port, prefix="jiak"):
        self.host = host
        self.port = port
        self.prefix = prefix
        if HAS_PYCURL:
            self._request = self._pycurl_request
        else:
            self._request = self._httplib_request
    
    def _build_path(self, bucket, key=''):
        return 'http://%s:%d/%s/%s/%s' % (self.host, self.port, self.prefix,
                                            urllib.quote_plus(bucket),
                                            urllib.quote_plus(key))
    
    def _httplib_request(self, method, uri, body="", headers={}):
        client = httplib.HTTPConnection(self.host, self.port)
        client.request(method, uri, body, headers)
        response = client.getresponse()
        return response.status, response.getheaders(), response
        
    def _pycurl_request(self, method, uri, body="", headers={}):
        resp_headers = StringIO()
        response = StringIO()
        client = pycurl.Curl()
        if method in ("PUT", "POST"):
            if method == "POST":
                client.setopt(pycurl.POST, 1)
            else:
                client.setopt(pycurl.CUSTOMREQUEST, method)
            client.setopt(pycurl.POSTFIELDS, body)
        elif method in ("DELETE",):
            client.setopt(pycurl.CUSTOMREQUEST, method)
        client.setopt(pycurl.URL, uri)
        client.setopt(pycurl.HTTPHEADER, build_headers(headers))
        client.setopt(pycurl.WRITEFUNCTION, response.write)
        client.setopt(pycurl.HEADERFUNCTION, resp_headers.write)
        client.perform()
        code = client.getinfo(pycurl.HTTP_CODE)
        
        return code, resp_headers, response
    
    @expect(204)
    def set_bucket_schema(self, bucket, allowed_fields, required_fields=[], write_mask=None, read_mask=None):
        write_mask = allowed_fields if write_mask is None else write_mask
        read_mask = allowed_fields if read_mask is None else read_mask
        body = json.dumps(dict(
            schema=dict(
                allowed_fields=allowed_fields,
                required_fields=required_fields,
                write_mask=write_mask,
                read_mask=read_mask)))
        code, _, resp = self._request("PUT", self._build_path(bucket), body, {'Content-Type': "application/json"})
        
        return code, resp
    
    @expect(200)
    def list_bucket(self, bucket):
        code, _, resp = self._request("GET", self._build_path(bucket))
        return code, resp
     
    @expect(200)    
    def store(self, bucket, key, obj, links=[], w=2, dw=2):
        obj = dict(
            bucket=bucket,
            key=key,
            object=obj,
            links=links)
        code, _, resp = self._request("PUT", '%s?%s' % (self._build_path(bucket, key),
                                        urllib.urlencode(dict(
                                            returnbody='true',
                                            w=w,
                                            dw=dw
                                        ))), json.dumps(obj), {'Content-Type': 'application/json'})
        return code, resp
    
    @expect([200, 404])    
    def fetch(self, bucket, key, r=2):
        code, headers, resp = self._request("GET", '%s?r=%d' % (self._build_path(bucket, key), r))
        return code, resp
    
    @expect([204, 404])
    def delete(self, bucket, key, dw=2):
        code, _, resp = self._request("DELETE", '%s?dw=%d' % (self._build_path(bucket, key), dw))
        return code, resp
        
    @expect([200, 404])
    def walk(self, bucket, key, spec):
        """spec should be a list of tuples, each of the form:
           (bucket, tag, acc) where bucket is a string name of 
           a bucket, or "_" to match any bucket tag is a string 
           tag name, or "_" to match any link tag acc is either 
           the string "1" or "0"
       
           if the walk succeeds, this will return a list, where
           each element is a list of JiakObjects corresponding to
           a spec element that had acc == "1"
        
        """
        def build_spec(spec):
            return "/".join(['%s,%s,%s' % tuple(map(urllib.quote_plus, [b,t,a])) for b,t,a in spec])
        
        code, _, resp = self._request("GET", '%s/%s' % (self._build_path(bucket, key), build_spec(spec)))
        return code, resp

if __name__ == '__main__':
    import doctest
    doctest.testmod()