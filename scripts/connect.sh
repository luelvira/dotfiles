#!/usr/bin/env bash

gpg sshconections.txt.gpg
if [ $? -ne 0 ]; then
	exit $?
fi
text=$(cat sshconections.txt)
line=$(grep -in $1 sshconections.txt | cut -d':' -f 1)
content=$(echo "$text" | head -n $(( $line + 3)) | tail -n 3)
user=$(echo "$content" | head -n 1 | cut -d' ' -f2)
pass=$(echo "$content" | head -n 2 | tail -n 1 | cut -d' ' -f2)
ip=$(echo "$content" | tail -n 1 | cut -d' ' -f2)
rm sshconections.txt
printf $pass | xclip -sel clip
ssh $user@$ip 

