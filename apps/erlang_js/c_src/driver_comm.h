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
