# Iptables esame 11-07-2025

### Elimina tutte le regole esistenti (nat)
`iptables -F`
`iptables -t nat -F`

### Scarta tutto a meno che non sia esplicitamente permesso (INPUT, FORWARD):
`iptables -P INPUT DROP`
`iptables -P FORWARD DROP`

### Consenti pacchetti ICMP ricevuti su eth0 e eth1 (INPUT):
`iptables -A INPUT -p icmp -i eth0 -j ACCEPT`
`iptables -A INPUT -p icmp -i eth1 -j ACCEPT`

### Consenti pacchetti SSH (tcp/22) ricevuti su eth1 e provenienti esclusivamente dall’host amministrativo 192.168.30.200 (INPT)
`iptables -A INPUT -i eth1 -s 192.168.30.200 -p tcp --dport 22 -j ACCEPT`

### Consenti tutti i pacchetti ricevti su eth1 e in uscita su eth0 (FORWARD):
`iptables -A FORWARD -i eth1 -o eth0 -j ACCEPT`

### Consenti tutti i pacchett con stato ESTABLISHED,RELATED (FORWARD):
`iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT`

### MASQUERADE pacchetti in uscita da eth0 affinché gli host privati ricevano risposte da Internet (POSTROUTING)
`iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE`

### Applica DNAT ai pacchetti HTTP (tcp/80) ricevuti su eth0, inoltrandoli a 192.168.30.60:8080 e assicurandoti che possano raggiungere quell'host (PREROUTING):
`iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 80 -j DNAT --to-destination 192.168.30.60:8080`
`iptables -A FORWARD -o eth1 -p tcp -d 192.168.30.60 --dport 8080 -j ACCEPT`