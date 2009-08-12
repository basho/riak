#!/usr/bin/env python

import httplib
try:
    import json
except ImportError:
    import simplejson as json

class JiakClient:
    '''A Python interface for speaking to Riak.
    (the following doctest only works if you
     have a running riak cluster with
     {riak_web_ip, "127.0.0.1"}.
     {riak_web_port, 8999}.
    )

    Example usage:

    >>> JC = JiakClient("127.0.0.1",8999)
    >>> [JC.delete("jiak_example", key) for key in ["doctestkey","jroot","jleaf1","jleaf2","jleaf3"]]
    [None, None, None, None, None]
    >>> JO = JiakObject("jiak_example", "doctestkey")
    >>> JO.object["foo"] = 2
    >>> JC.store(JO)
    >>> JC.fetch("jiak_example", "doctestkey").object["foo"] == 2
    True
    >>> JRoot = JiakObject("jiak_example","jroot")
    >>> JRoot.object["foo"] = 0
    >>> JLeaf1 = JiakObject("jiak_example","jleaf1")
    >>> JLeaf1.object["foo"] = "in results"
    >>> JLeaf2 = JiakObject("jiak_example","jleaf2")
    >>> JLeaf2.object["foo"] = "in results"
    >>> JLeaf3 = JiakObject("jiak_example","jleaf3")
    >>> JLeaf3.object["foo"] = "not in results"
    >>> JRoot.links = [("jiak_example", "jleaf1", "tag_one"), ("jiak_example", "jleaf2", "tag_one"), ("jiak_example", "jleaf3", "tag_other")]
    >>> [JC.store(xobj) for xobj in [JRoot, JLeaf1, JLeaf2, JLeaf3]]
    [None, None, None, None]
    >>> [O.object["foo"] for O in JC.walk("jiak_example","jroot",[("jiak_example","tag_one","1")])[0]]
    [u'in results', u'in results']
    >>> [JC.delete("jiak_example", key) for key in ["doctestkey","jroot","jleaf1","jleaf2","jleaf3"]]
    [None, None, None, None, None]
    '''

    def __init__(self, IP, Port,
                 JiakPrefix="/jiak/", JaywalkPrefix="/jaywalker/"):
        self.IP = IP
        self.Port = Port
        self.JKP = JiakPrefix
    def _do_req(self, method, uri, body="", headers={}):
        C = httplib.HTTPConnection(self.IP, self.Port)
        C.request(method, uri, body, headers)
        return C.getresponse()
    def _expect(self, Status, Resp):
        if Resp.status == Status:
            return json.loads(Resp.read())
        raise JiakException(Resp.status, Resp.reason, Resp.read())
    def list_bucket(self, Bucket):
        return self._expect(200, self._do_req("GET", self.JKP + Bucket))
    def store(self, JObj):
        NewData = self._expect(200,
                     self._do_req("PUT",
                                  self.JKP + JObj.bucket + "/" + JObj.key
                                     + "?returnbody=true",
                                  JObj.to_json(),
                                  {"Content-Type": "application/json"}))
        JObj.update(NewData)
    def fetch(self, bucket, key):
        Resp = self._do_req("GET", self.JKP + bucket + "/" + key)
        if Resp.status == 404:
            return None
        Data = self._expect(200,Resp)
        Obj = JiakObject(bucket, key)
        Obj.update(Data)
        return Obj
    def delete(self, bucket, key):
        Resp = self._do_req("DELETE", self.JKP + bucket + "/" + key)
        if Resp.status == 404:
            return None
        elif Resp.status == 204:
            return None
        raise JiakException(Resp.status, Resp.reason, Resp.read())
    def walk(self, bucket, key, spec):
        # spec should be a list of tuples, each of the form:
        # (bucket, tag, acc) where
        # bucket is a string name of a bucket, or "_" to match any bucket
        # tag is a string tag name, or "_" to match any link tag
        # acc is either the string "1" or "0"
        #
        # if the walk succeeds, this will return a list, where each
        #  element is a list of JiakObjects corresponding to a spec
        #  element that had acc == "1"
        Resp = self._do_req("GET", self.JKP + bucket + "/" + key + "/"
                            + _convert_walk_spec(spec))
        if Resp.status == 404:
            return None
        Data = self._expect(200,Resp)
        objlists = Data['results']
        return _convert_objlists(objlists)

def _convert_objlists(objlists):
    return [[_make_object(objdata) for objdata in objlist]
            for objlist in objlists]

def _make_object(data):
    O = JiakObject(data['bucket'], data['key'])
    O.update(data)
    return O

def _convert_walk_spec(spec):
    return "/".join([b + "," + t + "," + a for (b,t,a) in spec])

class JiakObject:
    def __init__(self, bucket, key, links=None, obj=None):
        self.bucket = bucket
        self.key = key
        if links == None: links = []
        self.links = links
        if obj == None: obj = {}
        self.object = obj
    def update(self, Data):
        self.vclock = Data["vclock"]
        self.lastmod = Data["lastmod"]
        self.vtag = Data["vtag"]
        self.object = Data["object"]
        self.links = Data["links"]
    def to_json(self):
        return json.dumps(self.__dict__)


class JiakException(Exception): pass

