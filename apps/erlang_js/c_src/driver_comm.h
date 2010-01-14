/* author Kevin Smith <ksmith@basho.com>
   copyright 2009-2010 Basho Technologies

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. */

#ifndef __DRIVER_COMM__
#define __DRIVER_COMM__

int read_int32(char **data);

/* Command strings will be two characters long */
/* and must be freed via driver_free()         */
char *read_command(char **data);

/* Any string read with this function must be */
/* freed with driver_free()                   */
char *read_string(char **data);

#endif
