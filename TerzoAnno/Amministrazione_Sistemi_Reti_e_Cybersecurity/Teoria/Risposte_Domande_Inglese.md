### [Access Control and rootly powers](8-access-control-and-rootly-powers.md)

- ==What core rules govern the traditional UNIX permission model?==
	- The standard UNIX access control model as remained largely unchanged for decades. This model follows a few basic rules.
		  1. Access control decisions depend on which user (or, in some cases, the user's membership in a group) is attempting to perform an operation.
		  2. Object have owners. Files and processes are examples of objects. Owners have broad, but not necessarily unrestricted, control over their objects.
		  3. You own the objects you create.
		  4. The special user account `root` can act as the owner of any object
		  5. Only `root` can perform certain sensitive administrative operations.

- ==Which operations can only the file owner (or `root`) perform, and what permission bits can be set on a file?==
	- The owner can set the permission of the file. There are 4 types of permission bits.
		- `-` $\rightarrow$ the file is a regular file
		- `rw-` $\rightarrow$ the owner can read (`r`) and write (`w`)
		- `rw-` $\rightarrow$ the group can read (`r`) and write (`w`)
		- `r--` $\rightarrow$ the others can read (`r`)

- ==Which operations can only the process owner (or `root`) perform, and what identities are associated with a process?==
	- The owner of a process can send process signals and reduce process scheduling priority.
	- There are multiple identities associated with a process:
		- real UID and GID (who we really are) $\rightarrow$ are taken from `/etc/passwd` on login and do not typically change during a login session (`root` could do it)
		- Effective UID, effective GID and supplementary GIDs (used for file access permission checks) $\rightarrow$ determine the actual file access permissions
		- Saved IDs (used to enter and leave privileged mode) $\rightarrow$ saved set-UID contains a copy of the effective UID and a saved set-GID contains a copy of the effective GID

- ==What is set-UID execution, why does `passwd` need it, and what happens when a regular user runs `passwd`?==
	- Some programs may require to run with privileges that the user who run them does not have. An example is `passwd`, the program that let users change their password (?)

- ==Why is `sudo` generally preferred to direct `root` login or `su` for obtaining `root` privileges, and what are its main advantages and drawbacks?==
	- The `sudo` command takes as its argument a command line to be executed as `root` (or another restricted user)
		1. `sudo` looks into `/etc/sudoers`, which is lists the people who are authorized to use `sudo` and the commands they are allowed to run on each host.
		2. if the command is permitted, `sudo` prompts for the user's password
		3. if the password is correct `sudo` executes the command
	- Pros and cons:

| Pro                                                                                          | Con                                                                                             |
| -------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| Command logging                                                                              | Command logging can be easily subverted (`sudo su`, although `sudo su`would at least be logged) |
| Users can do stuff that requires root privileges without having unlimited root privileges    |                                                                                                 |
| Users do not have to know the `root` password because `sudo` prompts for the user's password | Any breach in the security of a sudoer's personal account can be equivalent to breaching `root` |
| Faster of both `su` and `root` login                                                         |                                                                                                 |
| Privileges can be revoked without changing the `root`password                                |                                                                                                 |
| A list of all users with `root` privileges is maintained                                     |                                                                                                 |
| Lower chance of a `root` shell left unattended                                               |                                                                                                 |
| A single file can control access for an entire network                                       |                                                                                                 |
### [The filesystem](10-the-filesystem.md)

- ==Which file types does UNIX support, and how do the nine permission bits (`rwx` for user, group, and other) govern the allowed operations on each type?== 
	- UNIX support this type of file:
		- Regular file $\rightarrow$ Symbol `-`
		- Directory $\rightarrow$ Symbol `d`
		- Symbolic Link $\rightarrow$ Symbol `l`
		- Character device file $\rightarrow$ Symbol `c`
		- Block device file $\rightarrow$ Symbol `b`
		- Named pipe $\rightarrow$ Symbol `p`
		- Local domain socket $\rightarrow$ Symbol `s`
	- Nine permission bits determine what operations can be performed on a file and by whom. Three triplets of permission bits define access for the owner (aka user) of the file, the group owners, and everyone else. Each triplet has three bits: a read bit (`r`), a write bit (`w`), and an execute bit (`x`)

| File type | `r`               | `w`                                                                   | `x`     |
| --------- | ----------------- | --------------------------------------------------------------------- | ------- |
| `-`       | Read              | Write                                                                 | Execute |
| `d`       | List the contents | Create, delete, and rename files (works only in combination with `x`) | Enter   |
| `l`       | n/a               | n/a                                                                   | n/a     |
| `c`       | Read              | Write                                                                 | n/a     |
| `b`       | Read              | Write                                                                 | n/a     |
| `p`       | Read              | Write                                                                 | n/a     |
| `s`       | Read              | Connect and write                                                     | n/a     |
  
- ==Why is a lazy unmount (`umount -l`) considered unsafe, which command lets you identify the processes that still hold references to the busy filesystem, and how can you perform a clean unmount instead?==
	- The lazy unmount option removes a filesystem from the naming hierarchy but does not truly unmount it until all existing file references have been closed. 
	- It's consider unsafe for this two reasons:
		- there is no guarantee that existing references will ever close on their own
		- Lazy unmounted filesystem present inconsistent semantics to the programs that are using them. 
  
- ==What are the purposes of the set-UID, set-GID, and sticky bits, to which regular files or directories does each apply, and how do they alter permission checks?==
	- set-UID is only apply on the regular file $\rightarrow$ Run an executable file with the owner's permissions.
	- set-GID
		- for regular file $\rightarrow$ Run an executable file with the group owner's permissions
		- for directories $\rightarrow$ Newly created files take on the group ownership of the directory rather than the default group of the user that created the file
	- The sticky bit applies to directories only. If the sticky bit is set on a directory, a file can only be deleted or renamed by the owner of the directory, the owner of the file, or `root`
  
- ==Who may change a file’s permission bits, which command can they use, and how is that command invoked?==
	- The `chmod` command is to change the permissions on a file. Only the owner and `root` can do it. The first argument of `chmod` is a specification of the permissions to be assigned, and the following arguments are names of files on which such permissions apply.
  
- ==Who may change a file’s (group) ownership, what rules must be satisfied, and which command performs the operation?==
	- The `chown` command is to change the ownership of a file. The first argument sets owner and/or group owner (`user:group`), and the following arguments are the files on which the ownership change applies to
	- Must be the owner of the file and belong to the target group to change group ownership (or just be `root`)

### [Networking](11-networking.md)

- ==What is ARP spoofing, which weaknesses in the ARP protocol does it exploit, and how does a MITM attack unfold in practice?==
	- When an IP packet is sento from one computer to another, the destination IP address must be resolved to a MAC address.
	- ARP come to play when:
		- someone does not know the MAC address of the destination IP broadcast an ARP request to the local network
		- `10.1.1.1` says its own MAC address in an ARP reply
		- upon receiving the ARP reply, `10.1.1.1` updates its cache of known neighbors
	- ARP is a stateless protocol and there is no authentication in it.
	- Anyone can send an unsolicited ARP reply that rewrites a victim's cache with false information.
  
- How can an attacker mount a MITM attack with ICMP redirect messages, and which weaknesses in the ICMP protocol make this possible?
  
- ==What is IP forwarding, and why is it usually unsafe to leave it enabled on hosts that are not intended to act as routers?==
	- A host that has IP forwarding can act as a router. This means that can accept third party packets on one network interface, match them into a gateway or destination host on another network interface, and retransmits the packets.
	- Hosts that forward packets can sometimes be coerced into compromising security by making external packets appear to have come from inside the network, this evading network scanners and packet filters.
  
- ==What is IP spoofing, and what defenses can be used against it?==
	- The source address on an IP packet is normally filled in by the kernel's TCP/IP implementation and the IP address of the host from which the packet was sent. However, if the software that creates the packet uses raw sockets, it can fill in any source address it likes.
	- The defenses that can be used against it are:
		- Block outgoing packets whose source address is not within your address space.
		- Protect against attackers forging the source address on external packets to fool your firewall into thinking that they originated on your internal network. In this regard, an heuristic that helps is uRPF.
  
- ==What is IPv4 source routing, and how can an attacker exploit it?==
	- IPv4 source routing provides a mechanism to specify an explicit series of gateways for packet to transit on the way to its destination.
	- Source routing bypasses the next-hop routing algorithm that runs at each gateway to determine how a packet should be forwarded. Someone could cleverly route a packet to make it appear to have originated within your network instead of the Internet, thus slipping through the firewall

### [Security](12-security.md)

- ==What does the CIA triad stand for in information security, and what does each principle mean?==
	- Confidentiality (C) $\rightarrow$ Access to information should be limited to those who are authorized to have it (privacy data)
	- Integrity (I) $\rightarrow$ Information is valid and has not been altered in unauthorized ways (authenticity and trustworthiness of information)
	- Availability (A) $\rightarrow$ Information must be accessible to authorized users when they need it; otherwise, the data has no value. Outages not caused by intruders also fall into the category of availability problems.
  
- ==What is social engineering, why is it particularly difficult to defend against, and what is one common form of this attack?==
	- Social engineering is the use of psychological influence to persuade people to take actions or reveal information. Social engineering exploits human factors rather than software vulnerabilities. The goal is information gathering, fraud, or system access.
	- It's particularly difficult to defend because human users (and administrators) of a computer system are the weakest links in the chain of the security and no amount of technology can protect against the user element.
	- One common form of this attack is Phishing, where attackers deceive people into executing malicious code or revealing sensitive information.
  
- ==What is a software vulnerability, what is a specific example of such a vulnerability, and how can open-source code review practices help in reducing these vulnerabilities?== 
	- A vulnerability is a flaw or weakness in a system's design, implementation, or management that can be exploited by an attack to compromise its security. 
	- Buffer overflows are an example of a software bug with complex security implications.
	- Open-source code is thought to lead to better security. This why the more people who can scrutinize the code, the greater the chance that someone will spot a security weakness.
  
- ==What is the difference between a DoS attack and a DDoS attack, and how do these attacks typically impact the targeted systems?==
	- A DoS attack come from a single source and aim to overwhelm a system, making unavailable to users. A DDoS attack aims to make a system unavailable to its intended users by temporarily or indefinitely disrupting the victim's availability.
	- To conduct a DDoS attack, attackers typically plant malicious code on unprotected devices outside the victim's network. This code lets the attackers remotely command and control these intermediary systems, which form a so called "botnet"
  
- ==What is insider abuse, and why is it often harder to detect than external attacks?==
	- Employees, contractors, and consultants are trusted agents of an organization and are granted special privileges. Sometimes these privileges are abused. Insiders can steal or reveal data, disrupt systems for financial gain, or create havoc for political reasons
	- This type of attack is often the hardest of all to detect. As most security measures guard against external threats, such measures are not effective against users who have been granted access
  
- ==What is a backup in the context of computer security, and what are the key recommendations for effectively managing backups?==
	- A backup is a copy of computer data and stored elsewhere so that it may be used to restore the original after a data loss event. Regular, tested system backups are an essential part of any site security plan. Backup fall into the "availability" bucket of the CIA triad.
	- Key recommendations are:
		- all filesystems are replicated
		- some backups are stored off-site
  
- ==What are computer viruses and worms, and what are the key differences between these two types of malware?==
	- A virus is a type of malware that, when executed, replicates itself by modifying computer programs and inserting its own code into those programs.
	- A whore is standalone malware that replicate itself to spread to other computers. Worms does not require host programs.
  
- ==What is a rootkit, how does it typically function, and why can it be particularly challenging to detect and remove?==
	- A rootkit is software, typically malicious, designed to enable access to a computer or an area of its software that is not otherwise allowed and often marks it existence or the existence of other software.
	- The most advanced rootkits are aware of common removal programs and make attempts to subvert them.
  
- ==What are the best practices and recommendations for creating secure passwords, managing passwords effectively, and implementing MFA?==
	- Technically speaking, the most secure password of a given length consists of a random sequence of letters, punctuation, and digits
	- A password vault is a piece of software (or a combination of software and hardware) that encrypts the passwords it stores. A user can then access the passwords stored in the vault with a single master password, which becomes the only password to remember
	- An MFA systems validate your identity both through something you know (a password or passphrase) and something you have (a physical device, fingerprint, etc.)
  
- ==What is symmetric key cryptography, how does it work, and what are its primary advantages and disadvantages?==
	- $A$ and $B$ share a secret key ($K_{AB}$) that they use to encrypt and decrypt messages. They must find a way to exchange the shared secret privately. Once they both know the key, they can reuse it as long as they wish. $C$ can only inspect (or interfere with) messages if he also has the key
	- Symmetric keys are relatively efficient in terms of CPU usage and the size of the encrypted payloads. As a result, symmetric cryptography is often used in applications where efficient encryption and decryption are necessary
  
- ==What is public key cryptography, how does it work, and what are its primary advantages and disadvantages?==
	- Public key ciphers rely on the mathematical concept of trapdoor functions, in which a value is easy to compute, and yet it is difficult to derive the steps that produced that value. The performance characteristics of asymmetric ciphers generally render them impractical for encrypting large quantities of data
  
- What is a digital signature, what is its purpose, and how can it be created using public key cryptography?
  
- What is a digital certificate, what purpose does it serve, and how is it typically obtained?
  
- What is a hash function, and what specific properties define a cryptographic hash function?
  
- What is a firewall, how does a two-stage firewall filtering scheme work, and what role does a DMZ play?