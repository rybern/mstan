while true; do
	instances=$(pgrep mstan-server)
	echo $instances
	if [ -z "$instances" ]; then
		date
		echo its down, upping
		./run-bg.sh
	fi
 	sleep 1
done
