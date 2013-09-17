#include <stdio.h>
#include <stdlib.h>

static long tarai(long, long, long);

int
main(int argc, char **argv)
{
    printf("%ld\n", tarai(20, 10, 5));
    exit(0);
}

static long
tarai(long x, long y, long z)
{
    if (x <= y) {
        return y;
    }
    else {
        return tarai(tarai(x - 1, y, z),
                     tarai(y - 1, z, x),
                     tarai(z - 1, x, y));
    }
}
