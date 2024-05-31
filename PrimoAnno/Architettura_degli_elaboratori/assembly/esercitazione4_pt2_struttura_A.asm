.data 
.align 0 	# la direttiva .allign 0 rimuove gli allineamenti in memoria (puo' essere fatto solo su una macchina hardware che supporta gli accessi non in linea) 
c: .byte 'c' 	# codice ascii 0x63
.align 1
s: .half 4 	# allocazione in memoria di 0x0004
