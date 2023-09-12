#!/bin/bash

x=1
y=1

if (( $(( x == y )) ));
then
x=$(( x + y ))
echo $x
else
while (( $(( 1 != 1 )) ));
do
run=$(( run + 1 ))
done
echo "shit not equal"
fi