/*
 * This file is provided to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package com.basho.riak.client.plain;

import com.basho.riak.client.RiakClient;
import com.basho.riak.client.response.RiakExceptionHandler;
import com.basho.riak.client.response.RiakIORuntimeException;
import com.basho.riak.client.response.RiakResponseRuntimeException;
import com.basho.riak.client.util.ClientUtils;

/**
 * Converts unchecked exceptions RiakIORuntimeException and
 * RiakResponseRuntimeException to checked exceptions RiakIOException and
 * RiakRuntimeException. Be careful that everywhere calling a {@link RiakClient}
 * with this handler installed contains the appropriate throws declaration.
 */
public class ConvertToCheckedExceptions implements RiakExceptionHandler {

    /**
     * Throws a checked {@link RiakIOException}
     */
    public void handle(RiakIORuntimeException e) {
        ClientUtils.throwChecked(new RiakIOException(e));
    }

    /**
     * Throws a checked {@link RiakResponseException}
     */
    public void handle(RiakResponseRuntimeException e) {
        ClientUtils.throwChecked(new RiakResponseException(e));
    }

}