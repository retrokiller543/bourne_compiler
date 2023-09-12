#!/bin/bash

x=5
y=10

if (( $(( x == y )) ));
then
    x=$(( x + y ))
else
    z=0
    while (( $(( z != 5 )) ));
    do
        z=$(( z + 1 ))
        echo $z
    done
fi

echo $x