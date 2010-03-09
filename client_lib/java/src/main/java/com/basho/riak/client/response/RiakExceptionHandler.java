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
package com.basho.riak.client.response;

/**
 * Allows clients to handle exceptions in a separate class rather than inline
 * with the requests. If an RiakExceptionHandler is installed (use the client's
 * setExceptionHandler() method), then exceptions RiakIOExceptions and
 * RiakResponseExceptions will be passed to the handler rather than being
 * thrown. If exceptions can be handled centrally by the caller, using an
 * exception handler can result in cleaner code by avoiding repeated try/catch
 * blocks for every operation.
 */
public interface RiakExceptionHandler {

    /** Handle exceptions caused by communication errors with the sever */
    public void handle(RiakIORuntimeException e);

    /** Handle exceptions caused by malformed responses from the sever */
    public void handle(RiakResponseRuntimeException e);

}
