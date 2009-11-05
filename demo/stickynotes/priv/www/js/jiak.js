// JiakClient assumes the presence of jQuery and JSON libraries
// (or at least compatible $.ajax and JSON.stringify functions)
//
// Usage: instantiate a JiakClient with proper options, then use
// the store, fetch, remove, and walk function to get access to
// objects from Jiak.
//
// Examples:
//   var Client = new JiakClient('/jiak/');
//   Client.fetch('note', '123', function(note) {
//      note.object.text = 'Hello World';
//      Client.store(note);
//   });
//
//   Client.store({'bucket':'note',
//                 'object':{'text':'a new note'},
//                 'links':[]},
//                function(note) {
//                  alert("new note's key: "+note.key);
//                });
//
//   Client.walk(['note', '456'],
//               [{'bucket':'person', 'tag':'author'}],
//               function(data) {
//                 var authors = data.results[0];
//                 alert("note's author is: "+
//                       authors[0].object.name);
//               });
//
// Default R, W, DW, and RW values are all 2.  To use other
// values, pass an options object of the form:
//    {r: 1, // value you want for R
//     w: 3, // value you want for W
//     dw:1, // value you want for DW
//     rw:1} // value you want for RW
// or, pass the values as parameters to the store, fetch
// and delete functions.

// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.    


function JiakClient(BaseUrl, Opts) {
    this.baseurl = BaseUrl;
    if (!(this.baseurl.slice(-1) == '/'))
        this.baseurl += '/';

    this.opts = Opts||{};

    // utility to convert an integer to base64-encoded 32-bits
    base64 = function(N) {
        var base64digits = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
        return base64digits[(N >>> 26)]
            +base64digits[((N >>> 20)&63)]
            +base64digits[((N >>> 14)&63)]
            +base64digits[((N >>> 8)&63)]
            +base64digits[((N >>> 2)&63)]
            +base64digits[((N << 4)&63)]
            +'==';
    }

    if (('clientId' in this.opts) && !!this.opts.clientId) {
        if (typeof this.opts.clientId == "number"
            && this.opts.clientId > 0 && this.opts.clientId < 4294967296) {
            this.opts.clientId = base64(this.opts.clientId);
        }
        //otherwise, just use whatever clientId was given
    } else {
        //choose a client id if the caller didn't provide one
        this.opts.clientId = base64(Math.floor(Math.random()*4294967296));
    }
}

JiakClient.prototype.store = function(Object, Callback, NoReturnBody, W, DW, R) {
    var cid = this.opts.clientId;
    var req = {
        contentType: "application/json",
        dataType: "json",
        beforeSend: function(xhr) {
            xhr.setRequestHeader("X-Riak-ClientId", cid);
        }
    };

    if (this.opts.alwaysPost || !Object.key)
        req.type = 'POST';
    else
        req.type = 'PUT';
    
    req.url = this.path(Object.bucket);
    if (Object.key) req.url += Object.key;
    
    var q = false;
    if (!(this.opts.noReturnBody || NoReturnBody)) {
        req.url += '?returnbody=true';
        q = true;
    }

    if (W || this.opts.w) {
        req.url += (q?'&':'?')+'w='+(W||this.opts.w);
        q = true;
    }

    if (DW || this.opts.dw) {
        req.url += (q?'&':'?')+'dw='+(DW||this.opts.dw);
        q = true;
    }

    if (R || this.opts.r)
        req.url += (q?'&':'?')+'r='+(R||this.opts.r);

    if (typeof Callback == 'function')
        req.success = Callback;

    req.data = JSON.stringify(Object);

    return $.ajax(req);
}

JiakClient.prototype.fetch = function(Bucket, Key, Callback, R) {
    return $.ajax({
        url:      this.path(Bucket, Key)+
                    ((R||this.opts.r)?('?r='+(R||this.opts.r)):''),
        dataType: "json",
        success:  Callback
    });
}

JiakClient.prototype.remove = function(Bucket, Key, Callback, RW) {
    var cid = this.opts.clientId;
    return $.ajax({
        type:    'DELETE',
        url:     this.path(Bucket, Key)+
                   ((RW||this.opts.rw)?('?rw='+(RW||this.opts.rw)):''),
        success: Callback,
        beforeSend: function(xhr) {
            xhr.setRequestHeader("X-Riak-ClientId", cid);
        }
    });
}

JiakClient.prototype.walk = function(Start, Spec, Callback, Nocache) {
    var req = {
        dataType: "json",
        success: Callback
    };

    // Start can be either and object with {bucket:B, key:K}
    // or a list with [Bucket, Key, ...]
    if ('bucket' in Start)
        req.url = this.path(Start.bucket, Start.key)+'/';
    else
        req.url = this.path(Start[0], Start[1])+'/';

    // Spec should be a list of objects with
    //    {bucket:B, tag:T, acc:A}
    // where B and T specify the bucket and tag to match in the link
    //   or are undefined to match anything
    // and A is 'true' to get the objects matched at this step, or
    //   false to have them excluded from the response (always true
    //   for the last step
    for (i in Spec) {
        req.url += encodeURIComponent(Spec[i].bucket||'_')+','+
            encodeURIComponent(Spec[i].tag||'_')+','+
            ((Spec[i].acc || i == Spec.length-1) ? '1' : '_')+'/';
    }

    if (Nocache) req.url += '?nocache='+(+new Date());

    return $.ajax(req);
}

JiakClient.prototype.setBucketSchema = function(Bucket, Schema, Callback) {
    if (!('required_fields' in Schema))
        Schema.required_fields = [];

    if (!('read_mask' in Schema))
        Schema.read_mask = Schema.allowed_fields;

    if (!('write_mask' in Schema))
        Schema.write_mask = Schema.read_mask;

    $.ajax({
        type:        'PUT',
        url:         this.path(Bucket),
        contentType: 'application/json',
        data:        JSON.stringify({schema:Schema}),
        success:     Callback ? function() { Callback(true); } : undefined,
        error:       Callback ? function() { Callback(false); } : undefined
    });
}

JiakClient.prototype.path = function(Bucket, Key) {
    var p = this.baseurl;
    if (Bucket) {
        p += encodeURIComponent(Bucket)+'/';
        if (Key)
            p += encodeURIComponent(Key);
    }
    return p;
}