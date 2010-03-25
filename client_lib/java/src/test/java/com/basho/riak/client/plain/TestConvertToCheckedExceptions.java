package com.basho.riak.client.plain;

import org.junit.Test;

import com.basho.riak.client.response.RiakIORuntimeException;
import com.basho.riak.client.response.RiakResponseRuntimeException;

public class TestConvertToCheckedExceptions {

    ConvertToCheckedExceptions impl = new ConvertToCheckedExceptions();
    
    @Test(expected=RiakIOException.class) public void translates_io_runtime_exception_to_checked() throws RiakIOException {
        impl.handle(new RiakIORuntimeException());
    }
    @Test(expected=RiakResponseException.class) public void translates_response_runtime_exception_to_checked() throws RiakResponseException {
        impl.handle(new RiakResponseRuntimeException(null));
    }
}