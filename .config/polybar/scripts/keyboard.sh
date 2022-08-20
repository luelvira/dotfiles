#!/usr/bin/env bash

case "$(xset -q | grep LED | awk '{print $10 }')" in \
	"00000000") KDB="EN" ;;
	"00000001") KDB="EN" ;;
	"00000002") KDB="EN" ;;
	"00000003") KDB="EN" ;;
	"00001004") KDB="ES" ;;
	"00001005") KDB="ES" ;;
	"00001006") KDB="ES" ;;
	"00001007") KDB="ES" ;;
	* ) KDB="unknown" ;;
 esac
 echo $KDB
