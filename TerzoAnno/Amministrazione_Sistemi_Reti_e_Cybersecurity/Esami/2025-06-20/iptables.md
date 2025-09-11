# Iptables esame 2025-06-20

### Elimina tutte le regole esistenti (filter, nat):
`iptables -F`
`iptables -t nat -F`

### Scarta tutto a meno che non sia esplicitamente permesso (INPUT, FORWARD):
`iptables -P INPUT DROP`
`iptables -P FORWARD DROP`

### Consenti pacchetti ICMP ricevuti su eth0 e eth1 (INPUT):
`iptables -A INPUT -i eth0 -p icmp -j ACCEPT`
`iptables -A INPUT -i eth1 -p icmp -j ACCEPT`

### Consenti pacchetti SSH (tcp/22) ricevuti su eth1 (INPUT):
`iptabless -A INPUT -i eth1 -p tcp --dport 22 -j ACCEPT`

### Consenti tutti i pacchetti ricevuti su eth1 e in uscita su eth0 (FORWARD)
`iptables -A FORWARD -i eth0 -o eth1 -j ACCEPT`

### Consenti pacchetti con stato ESTABLISHED,RELATED (FORWARD):
`iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT`

### MASQUERADE pacchetti in uscita da eth0 affinché gli host privati ricevano risposte da Internet (POSTROUTING):
`iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE`

### Applica DNAT ai pacchetti FTPS (tcp/990) ricevuti su eth0 inoltrandoli a 10.10.20.50:19990 (PREROUTING):
`iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 990 -j DNAT --to-destination 10.10.20.50:19990`
`iptables -A FORWARD -i eth0 -o eth1 -p tcp -d 10.10.20.50 --dport 19990 -j ACCEPT`