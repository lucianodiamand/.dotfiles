#!/usr/bin/env fish
set text (upower -i /org/freedesktop/UPower/devices/battery_$argv | grep -E "state|to\ full|percentage"|awk '{print $2}' |tr -d %)
set state $text[1..-2]
set percentage $text[-1]
if [ $state[1] = charging ]
  set start F00B
else
  set start F00C
end
set hex $start(printf '%x' (echo "$percentage / 10"|bc))
printf "\U$hex  $state $percentage%%" | cut -c1-90
