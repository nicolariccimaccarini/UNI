# Iptables mock exam

### Flush existing rules (nat):
`iptables -F`
`iptables -t nat -F`

### Drop everything unless explicity allowed (INPUT, FORWARD):
`iptables -P INPUT DROP`
`iptables -P FORWARD DROP`

### Allow ICMP packets received on eth0 and eth1 (INPUT):
`iptables -A INPUT -i eth0 -p icmp -j ACCEPT`
`iptables -A INPUT -i eth1 -p icmp -j ACCEPT`

### Allow SSH packets (tcp/22) received on eth1 (INPUT):
`iptables -A INPUT -i eth1 -p tcp --dport 22 -j ACCEPT`

### Allow all packets received on eth1 (FORWARD):
`iptables -A FORWARD -i eht1 -j ACCEPT`

### Allow ESTABLISHED,RELATED packets (FORWARD):
`iptables -A FORWARD -m state --state ESTABLISHED,RELATED -j ACCEPT`

oppure

`iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT`

### SNAT for packets to be sent on eth0 so that private hosts get replies from the Internet (POSTROUTING):
`iptables -t nat POSTROUTING -o eth0 -j SNAT --to-source 93.184.216.32

### DNAT for HTTP packets (tcp/80) received on eth0 to 10.10.10.2:30080 (PREROUTING):
`iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 80 -j DNAT --to-destination 10.10.10.3:30080`
`iptables -A FORWARD -i eth0 -o eth1 -p tcp -d 10.10.10.2 --dport 30080 -j ACCEPT`