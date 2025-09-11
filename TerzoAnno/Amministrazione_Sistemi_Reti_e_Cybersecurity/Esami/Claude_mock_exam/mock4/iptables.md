### Elimina tutte le regole esistenti (filter, nat):
`iptables -F`
`iptables -t nat -F`

### Scarta tutto a meno che non sia esplicitamente permesso (INPUT, FORWARD):
`iptables -P INPUT DROP`
`iptables -P FORWARD DROP`

### Consenti pacchetti ICMP ricevuti su eth1 (INPUT):
`iptables -A INPUT -i eht1 -p icmp -j ACCEPT`

### Consenti pacchetti SSH (`tcp/22`) ricevuti su `eth1` solo da `10.0.1.100 (INPUT):
`iptables -A INPUT -i eth1 -p tcp --dport 22 -s 10.0.1.100 -j ACCEPT`

### Consenti pacchetti DNS (`udp/53`) ricevuti su `eth1 (FORWARD):
`iptables -A FORWARD -i eth1 -p udp --dport 53 -j ACCEPT`

### Consenti pacchetti con stato `ESTABLISHED,RELATED (FORWARD):
`iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT`

### SNAT per pacchetti in uscita su `eth0` usando l'IP del firewall (POSTROUTING):
`iptables -t nat -A POSTROUTING -o eth0 -j SNAT --to-source 198.51.100.5`

### DNAT per pacchetti MySQL (`tcp/3306`) ricevuti su `eth0` verso `10.0.1.50:3306` (PREROUTING)
`iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 3306 -j DNAT --to-destination 10.0.1.50:3306`
`iptables -A FORWARD -i eth0 -o eth1 -p tcp -d 10.0.1.50 --dport 3306 -j ACCEPT`