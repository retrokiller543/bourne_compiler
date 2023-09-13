#!/bin/bash
x=10
y=20
z=$(( x + y ))
echo Sum of x and y is:
echo $z
if (( $(( z == 30 )) ));
then
echo z is 30
else
echo z is not 30
fi
while (( $(( x != 0 )) ));
do
echo Counting down: 
echo $x
x=$(( x - 1 ))
done
echo Done counting!