#!/usr/bin/env python
"""
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

import urllib
import base64
import random
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
            code, headers, resp = f(*args, **kwargs)
            if HAS_PYCURL:
                resp.reset()
            if code not in status:
                raise RiakException(code, resp.read())
            if code in [404, 204]:
                return None
            else:
                try:
                    resp = json.load(resp)
                except:
                    pass
                obj = {'object': resp}
                for k,v in headers:
                    if k == 'x-riak-vclock':
                        obj['__riak_vclock__'] = v
                return obj
        return wrapped_func
    return wrapper_func

def build_headers(h, disable_continue=True):
    headers = ['Expect:'] if disable_continue else []
    for k,v in h.iteritems():
        headers.append('%s: %s' % (k, v))
    return headers

class RiakException(Exception): pass

class Riak(object):
    """A Python interface for the Riak (http://riak.basho.com/) key-value store.

       Example Usage:
       
       >>> client = Riak('127.0.0.1', 8098, 'raw')
       >>> client.delete('raw_example', 'doctestkey')
       >>> obj = client.store('raw_example', 'doctestkey', {'foo':2})
       >>> client.fetch('raw_example', 'doctestkey').get('object', {}).get('foo') == 2
       True
       
    """
    def __init__(self, host, port, prefix="raw"):
        self.host = host
        self.port = port
        self.prefix = prefix
        self.clientid = 'py_' + base64.b64encode(str(
                                        random.randint(1,1073741824)))
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
    
    @expect(200)
    def list_bucket(self, bucket):
        code, headers, resp = self._request("GET", self._build_path(bucket))
        return code, headers, resp
     
    @expect(200)    
    def store(self, bucket, key, obj, links=[], w=2, dw=2):
        uphead = {'Content-Type': 'application/json',
                  'X-Riak-ClientId': self.clientid}
        try:
            uphead['X-Riak-Vclock'] = obj['__riak_vclock__']
        except KeyError:
            pass
        try:
            content = obj['object']
        except:
            content = obj
        code, headers, resp = self._request("PUT", '%s?%s' % (self._build_path(bucket, key),
                                        urllib.urlencode(dict(
                                            returnbody='true',
                                            w=w,
                                            dw=dw
                                        ))), json.dumps(content), uphead)
        return code, headers, resp
    
    @expect([200, 404])    
    def fetch(self, bucket, key, r=2):
        code, headers, resp = self._request("GET", '%s?r=%d' % (self._build_path(bucket, key), r))
        return code, headers, resp
    
    @expect([204, 404])
    def delete(self, bucket, key, dw=2):
        code, headers, resp = self._request("DELETE", '%s?dw=%d' % (self._build_path(bucket, key), dw))
        return code, headers, resp
        
    @expect([200, 404])
    def walk(self, bucket, key, spec):
        """spec should be a list of tuples, each of the form:
           (bucket, tag, acc) where bucket is a string name of 
           a bucket, or "_" to match any bucket tag is a string 
           tag name, or "_" to match any link tag acc is either 
           the string "1" or "0"
       
           if the walk succeeds, this will return a list, where
           each element is a list of RiakObjects corresponding to
           a spec element that had acc == "1"
        
        """
        def build_spec(spec):
            return "/".join(['%s,%s,%s' % tuple(map(urllib.quote_plus, [b,t,a])) for b,t,a in spec])
        
        code, headers, resp = self._request("GET", '%s/%s' % (self._build_path(bucket, key), build_spec(spec)))
        return code, headers, resp

if __name__ == '__main__':
    import doctest
    doctest.testmod()
