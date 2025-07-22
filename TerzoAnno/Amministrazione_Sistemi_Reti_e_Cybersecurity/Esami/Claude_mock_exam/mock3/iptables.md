### Elimina tutte le regole esistenti
`iptables -F`
`iptables -t nat -F`

### Scarta tutto a meno che non sia esplicitamente permesso
`iptables -A INPUT -P DROP`
`iptables -A FORWARD -P DROP`

### Consenti pacchetti ICMP ricevuti su eth1
`iptables -A INPUT -i eth1 -p icmp -j ACCEPT`

### Consenti pacchetti SSH ( tcp/22 ) ricevuti su eth1
`iptables -A INPUT-i eth1 -p tcp --dport 22 -j ACCEPT`

### Consenti pacchetti DNS ( udp/53 e tcp/53 ) ricevuti su eth1
`iptables -A INPUT -i eth1 -p udp --dport 53 -j ACCEPT`
`iptables -A INPUT -i eth1 -p tcp --dport 53 -j ACCEPT`

### Consenti pacchetti con stato ESTABLISHED,RELATED
`iptables -A FORWARD -m state --state ESTABLISHED,RELATED -j ACCEPT`

### MASQUERADE pacchetti in uscita su eth0 per consentire l'accesso a Internet agli host privati
`iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE`

### DNAT per pacchetti DNS ( udp/53 ) ricevuti su eth0 verso 172.20.0.25:53
`iptables -t nat -A POSTROUTING -i eth0 -p udp --dport 53 -j DNAT --to-destination 172.20.0.25:53`

### DNAT per pacchetti DNS ( tcp/53 ) ricevuti su eth0 verso 172.20.0.25:53
`iptables -t nat -A POSTROUTING -i eth0 -p tcp --dport 53 -j DNAT --to-destination 172.20.0.25:53`