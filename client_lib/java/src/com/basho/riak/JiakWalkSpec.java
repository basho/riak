/*
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
 */
package com.basho.riak;

import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.StringTokenizer;

/* JiakWalkSpecStep is the internal representation of a JiakWalkSpec
 * segment.  It should not be used directly.
 */
class JiakWalkSpecStep {
    public final String bucket;
    public final String tag;
    public final String accumulateFlag;

    public JiakWalkSpecStep(final String b, final String t, final String a) {
        bucket = b; tag = t; accumulateFlag = a;
    }
}

/**
 * Tool for building Jiak/Jaywalker specs.  Using this class to
 * specify walk specs ensures that bucket and tag names will be
 * properly URL-escaped.
 *
 * @author Bryan Fink <bryan@basho.com>
 * @version 0.1
 */
public class JiakWalkSpec extends ArrayList<JiakWalkSpecStep> {
    /**
     * The "don't care" signifier.  Pass this as the bucket, tag,
     * or accumulate flag to select the default, or match-all option.
     */
    public static final String WILDCARD = "_";

    /**
     * Create an empty Jiak walk spec.
     */
    public JiakWalkSpec() {
        super();
    }

    /**
     * Append a step to this walk spec.
     *
     * @param bucket
     *            The bucket of the step, or the wildcard.
     * @param tag
     *            The tag of the step, or the wildcard.
     * @param accumulateFlag
     *            The string "1" to force this step to be accumulated
     *            in the results.  "0" to force this step not to be
     *            accumulated.  WILDCARD to accept the default
     *            accumulation setting ("yes" for the last step, "no"
     *            for all others).
     */
    public void addStep(final String bucket,
                        final String tag,
                        final String accumulateFlag) {
        this.add(new JiakWalkSpecStep(bucket, tag, accumulateFlag));
    }

    /**
     * Append a step to this walk spec.  Same as the other addStep
     * function, but allows the use of a boolean instead of a string
     * for the accumulateFlag parameter.
     */
    public void addStep(final String bucket,
                        final String tag,
                        boolean accumulateFlag) {
        addStep(bucket, tag, accumulateFlag ? "1" : "0");
    }

    /**
     * Append a step to this walk spec, and take the default option
     * for the accumulate flag.
     */
    public void addStep(final String bucket, final String tag) {
        addStep(bucket, tag, WILDCARD);
    }

    /**
     * Convert this walk step to a string.  All bucket and tag names
     * will be URL-escaped in the return value.
     */
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (JiakWalkSpecStep s : this) {
            result.append(URLEncoder.encode(s.bucket));
            result.append(',');
            result.append(URLEncoder.encode(s.tag));
            result.append(',');
            result.append(s.accumulateFlag);
            result.append('/');
        }
        return result.toString();
    }
}
