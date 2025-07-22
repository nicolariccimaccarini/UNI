### Elimina tutte le regole esistenti
`iptables -F`
`iptables -t nat -F`

### Scarta tutto a meno che non sia esplicitamente permesso
`iptables -P INPUT DROP`
`iptables -P FORWARD DROP`

### Consenti pacchetti ICMP ricevuti su eth1
`iptables -A INPUT -i eth1 -p icmp -j ACCEPT`

### Consenti pacchetti SSH ( tcp/22 ) ricevuti su eth1
`iptables -A INPUT -i eth1 -p tcp --dport 22 -j ACCEPT`

### Consenti tutti i pacchetti ricevuti su eth1 e diretti su eth0
`iptables -A FORWARD -i eth1 -o eth0 -j ACCEPT`

### Consenti pacchetti con stato ESTABLISHED,RELATED
`iptables -A FORWARD -m state --state ESTABLISHED,RELATED -j ACCEPT`

### SNAT per pacchetti in uscita su eth0 utilizzando l'IP del firewall
`iptables -t nat -A POSTROUTING -o eth0 -j SNAT --to-source 203.0.113.5`

### DNAT per pacchetti HTTP ( tcp/80 ) ricevuti su eth0 verso 10.0.0.50:80
`iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 80 -j DNAT --to-destination 10.0.0.50:80`

### DNAT per pacchetti HTTPS ( tcp/443 ) ricevuti su eth0 verso 10.0.0.50:443
`iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 443 -j DNAT --to-destination 10.0.0.50:443`

### DNAT per pacchetti SMTP ( tcp/25 ) ricevuti su eth0 verso 10.0.0.60:25
`iptables -t nat -A PREROUTING -i eth0 -p tcp --dport 25 -j DNAT --to-destination 10.0.0.60:25