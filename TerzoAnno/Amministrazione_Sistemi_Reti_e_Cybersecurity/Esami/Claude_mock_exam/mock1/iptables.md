### Elimina tutte le regole esistenti
`iptables -F`
`iptables -t nat -F`

### Scarta tutto a meno che non sia esplicitamente permesso
`iptables -P INPUT DROP`
`iptables -P FORWARD DROP`

### Consenti pacchetti ICMP ricevuti su eth0 e eth1
`iptables -A INPUT -i eth0 -p icmp -j ACCEPT`
`iptables -A INPUT -i eth1 -p icmp -j ACCEPT`

### Consenti pacchetti SSH ( tcp/22 ) ricevuti su eth1
`iptables -A INPUT -i eth1 -p tcp --dport 22 -j ACCEPT`

### Consenti pacchetti HTTP ( tcp/80 ) e HTTPS ( tcp/443 ) ricevuti su eth1
`iptables -A FORWARD -i eth1 -p tcp --dport 80 -j ACCEPT`
`iptables -A FORWARD -i eth1 -p tcp --dport 443 -j ACCEPT`

###  Consenti pacchetti con stato ESTABLISHED,RELATED
`iptables -A FORWARD -m state --state ESTABLISHED,RELATED -j ACCEPT`

### MASQUERADE pacchetti in uscita su eth0 per consentire l'accesso a Internet agli host privati
`iptables -t nat -A POSTROUTING -o eht0 -j MASQUERADE`

### DNAT per pacchetti SSH ( tcp/22 ) ricevuti su eth0 verso 192.168.1.100:2222
`iptables -t nat -a PREROUTING -i eth0 -p tcp --dport 22 -j DNAT --to-destination 192.168.1.100:2222`