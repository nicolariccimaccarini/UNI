# nome e cognome: Nicola Ricci Maccarini
# matricola: 185792

### Elimina le regole esistenti:
`iptables -F`
`iptables -t nat -F`

### Scarta tutto a meno che non sia esplicitamente permesso
`iptables -A INPUT DROP`
`iptables -A FORWARD DROP`

### Consenti pacchetti ICMP ricevuti su eth1:
`iptables -A INPUT -i eth1 -p icmp -j ACCEPT`

### Consenti pacchetti SSH (tcp/22) ricevuti su eth1:
`iptables -A INPUT -i eth1 -p tcp --dport 22 -j ACCEPT`

### Consenti paccheti HTTP (tcp/80) e HTTPS (tcp/443) ricevuti su eth0 e eth1
`iptables -A FORWARD -i eth0 -p tcp --dport 80 -j ACCEPT`
`iptables -A FORWARD -i eth0 -p tcp --dport 443 -j ACCEPT`
`iptables -A FORWARD -i eth1 -p tcp --dport 80 -j ACCEPT`
`iptables -A FORWARD -i eth1 -p tcp --dport 443 -j ACCEPT`

### Consenti pacchett con stato ESTABLISHED,RELATED
`iptables -A FORWARD -m state --state ESTABLISHED,RELATED -j ACCEPT

### SNAT per i pacchetti in uscita su eth0 affinche' gli host privati ricevano risposte da internet
`iptables -t nat -A POSTROUTING -o eth0 -j SNAT --to-source 203.0.113.10

### DNAT per pacchetti HTTPS (tcp/443) ricevuti su eth0 verso 192.168.50.20:30443
`iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 443 -j DNAT --to-destination 192.168.50.20:30443`
`iptables -A FORWARD -i eth0 -o eth1 -p tcp -d 192.168.50.20 --dport 30443 -j ACCEPT`