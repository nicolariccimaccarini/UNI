### [Access Control and rootly powers](8-access-control-and-rootly-powers.md)

- ==What core rules govern the traditional UNIX permission model?==
    - The standard UNIX access control model has remained largely unchanged for decades. This model follows a few basic rules:
          1. Access control decisions depend on which user (or, in some cases, the user's membership in a group) is attempting to perform an operation.
          2. Objects have owners. Files and processes are examples of objects. Owners have broad, but not necessarily unrestricted, control over their objects.
          3. You own the objects you create.
          4. The special user account `root` can act as the owner of any object
          5. Only `root` can perform certain sensitive administrative operations.

- ==Which operations can only the file owner (or `root`) perform, and what permission bits can be set on a file?==
    - Only the file owner (or `root`) can set the permissions of the file. The permission bits that can be set are:
        - **Read (`r`)**: Permission to read the file content
        - **Write (`w`)**: Permission to modify or delete the file
        - **Execute (`x`)**: Permission to execute the file (for executables) or enter the directory
        - **Set-UID**: Run executable with owner's permissions
        - **Set-GID**: Run executable with group owner's permissions  
        - **Sticky bit**: (For directories) Only file owner, directory owner, or `root` can delete files

- ==Which operations can only the process owner (or `root`) perform, and what identities are associated with a process?==
    - The owner of a process can:
        - Send process signals to the process
        - Reduce process scheduling priority (increase niceness)
    - There are multiple identities associated with a process:
        - **Real UID and GID**: Who we really are - taken from `/etc/passwd` on login and do not typically change during a login session (`root` could change them)
        - **Effective UID, effective GID and supplementary GIDs**: Used for file access permission checks - determine the actual file access permissions
        - **Saved IDs**: Used to enter and leave privileged mode - saved set-UID contains a copy of the effective UID and saved set-GID contains a copy of the effective GID

- ==What is set-UID execution, why does `passwd` need it, and what happens when a regular user runs `passwd`?==
    - **Set-UID execution**: When the kernel runs an executable file with the set-UID bit set, it changes the effective UID of the process to the owner of the file instead of the UID of the user who ran the command.
    - **Why `passwd` needs it**: The `passwd` command needs to modify `/etc/shadow` (which is only writable by `root`) to change user passwords. Regular users don't have permission to write to this file.
    - **What happens when a regular user runs `passwd`**:
        1. User `ubuntu` runs `passwd` (which is owned by `root` and has set-UID bit set)
        2. The process runs with effective UID = 0 (`root`) instead of 1000 (`ubuntu`)
        3. The process can now read and write `/etc/shadow` with `root` privileges
        4. User can change their password without having direct access to the shadow file

- ==Why is `sudo` generally preferred to direct `root` login or `su` for obtaining `root` privileges, and what are its main advantages and drawbacks?==
    - **How `sudo` works**:
        1. `sudo` looks into `/etc/sudoers`, which lists the people who are authorized to use `sudo` and the commands they are allowed to run on each host.
        2. If the command is permitted, `sudo` prompts for the **user's password** (not root password)
        3. If the password is correct, `sudo` executes the command
    - **Advantages and disadvantages**:

| Pro                                                                                          | Con                                                                                             |
| -------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- |
| Command logging                                                                              | Command logging can be easily subverted (`sudo su`, although `sudo su` would at least be logged) |
| Users can do stuff that requires root privileges without having unlimited root privileges    |                                                                                                 |
| Users do not have to know the `root` password because `sudo` prompts for the user's password | Any breach in the security of a sudoer's personal account can be equivalent to breaching `root` |
| Faster than both `su` and `root` login                                                         |                                                                                                 |
| Privileges can be revoked without changing the `root` password                                |                                                                                                 |
| A list of all users with `root` privileges is maintained                                     |                                                                                                 |
| Lower chance of a `root` shell left unattended                                               |                                                                                                 |
| A single file can control access for an entire network                                       |                                                                                                 |

### [The filesystem](10-the-filesystem.md)

- ==Which file types does UNIX support, and how do the nine permission bits (`rwx` for user, group, and other) govern the allowed operations on each type?== 
    - **UNIX file types**:
        - Regular file → Symbol `-`
        - Directory → Symbol `d`
        - Symbolic Link → Symbol `l`
        - Character device file → Symbol `c`
        - Block device file → Symbol `b`
        - Named pipe → Symbol `p`
        - Local domain socket → Symbol `s`
    - **Nine permission bits**: Three triplets of permission bits define access for the owner (user), group owners, and everyone else. Each triplet has read (`r`), write (`w`), and execute (`x`) bits:

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
    - **Why lazy unmount is unsafe**: 
        - There is no guarantee that existing references will ever close on their own
        - Lazy unmounted filesystems present inconsistent semantics to the programs that are using them
    - **Command to identify processes**: `fuser -v -m /path/to/filesystem` shows which processes are using the mounted filesystem
    - **Clean unmount**: First identify and stop processes using the filesystem, then use regular `umount /path/to/filesystem`
  
- ==What are the purposes of the set-UID, set-GID, and sticky bits, to which regular files or directories does each apply, and how do they alter permission checks?==
    - **Set-UID bit**:
        - Applies to: Regular files only
        - Purpose: Run an executable file with the owner's permissions instead of the user who executed it
    - **Set-GID bit**:
        - For regular files: Run an executable file with the group owner's permissions
        - For directories: Newly created files take on the group ownership of the directory rather than the default group of the user that created the file
    - **Sticky bit**:
        - Applies to: Directories only
        - Purpose: If set on a directory, a file can only be deleted or renamed by the owner of the directory, the owner of the file, or `root`
  
- ==Who may change a file's permission bits, which command can they use, and how is that command invoked?==
    - **Who can change**: Only the file owner and `root` can change file permissions
    - **Command**: `chmod` (change mode)
    - **Invocation**: The first argument specifies the permissions (octal or mnemonic syntax), followed by the file names
        - Octal example: `chmod 755 filename` 
        - Mnemonic example: `chmod u+x,g-w filename`
  
- ==Who may change a file's (group) ownership, what rules must be satisfied, and which command performs the operation?==
    - **Who can change**: Only `root` can change file ownership. Regular users can only change group ownership under specific conditions.
    - **Rules for group ownership change**: 
        - Must be the owner of the file AND belong to the target group (or be `root`)
        - Cannot give away ownership of files (except `root`)
    - **Command**: `chown user:group filename` to change both owner and group, or `chown :group filename` to change only group

### [Networking](11-networking.md)

- ==What is ARP spoofing, which weaknesses in the ARP protocol does it exploit, and how does a MITM attack unfold in practice?==
    - **ARP spoofing**: An attack where the attacker sends spoofed ARP replies to associate their MAC address with the IP address of another host (typically the gateway).
    - **ARP protocol weaknesses**:
        - ARP is a stateless protocol - ARP replies are automatically cached regardless of whether they follow an ARP request
        - There is no authentication in ARP
        - Anyone can send unsolicited ARP replies that rewrite a victim's cache with false information
    - **MITM attack process**:
        1. Attacker sends malicious ARP reply to victim: "I am 10.1.1.1 (gateway) at attacker_MAC"
        2. Victim updates ARP cache, now thinking attacker's MAC is the gateway
        3. Attacker sends ARP reply to gateway: "I am 10.1.1.10 (victim) at attacker_MAC"  
        4. All traffic between victim and gateway now flows through attacker
        5. Attacker can intercept, modify, or analyze all communications
  
- ==How can an attacker mount a MITM attack with ICMP redirect messages, and which weaknesses in the ICMP protocol make this possible?==
    - **ICMP redirect MITM attack**:
        - Attacker sends malicious ICMP redirect message to victim claiming to come from an external host
        - The redirect message tells victim: "you should send packets for host X to me instead of the current gateway"
        - Victim's system automatically adjusts its routing table accordingly
        - Future traffic gets routed through the attacker, allowing interception
    - **ICMP protocol weaknesses**:
        - **Lack of Authentication**: ICMP redirects contain no authentication information - no way to verify the message comes from a legitimate router
        - **Automatic Processing**: When a host receives an ICMP redirect, it automatically adjusts its routing table without additional verification

- ==What is IP forwarding, and why is it usually unsafe to leave it enabled on hosts that are not intended to act as routers?==
    - **IP forwarding**: A host with IP forwarding enabled can act as a router - it can accept third-party packets on one network interface, match them to a gateway or destination host on another network interface, and retransmit the packets.
    - **Why it's unsafe**: 
        - Hosts that forward packets can be coerced into compromising security by making external packets appear to have come from inside the network
        - This can evade network scanners and packet filters
        - Can be exploited through ARP spoofing and ICMP redirects to redirect traffic through compromised hosts
        - Unless the system is intended to function as a router, IP forwarding should be disabled
  
- ==What is IP spoofing, and what defenses can be used against it?==
    - **IP spoofing**: The practice of creating IP packets with a false source IP address. Normally the kernel fills in the source address, but with raw sockets, software can specify any source address.
    - **Defenses**:
        - **Egress filtering**: Block outgoing packets whose source address is not within your address space
        - **Ingress filtering**: Protect against attackers forging external packets to appear internal
        - **uRPF (Unicast Reverse Path Forwarding)**: Heuristic that helps verify packet source addresses by checking if the source is reachable via the interface it arrived on
  
- ==What is IPv4 source routing, and how can an attacker exploit it?==
    - **IPv4 source routing**: A mechanism to specify an explicit series of gateways for a packet to transit on the way to its destination, bypassing normal next-hop routing algorithms.
    - **Attacker exploitation**:
        - Attacker can cleverly route a packet to make it appear to have originated within your internal network instead of the Internet
        - This allows the packet to slip through firewalls that block external traffic
        - Can be used to bypass security measures by making external attacks appear as internal traffic
        - Most systems drop source-routed packets by default as a security measure

### [Security](12-security.md)

- ==What does the CIA triad stand for in information security, and what does each principle mean?==
    - **Confidentiality (C)**: Access to information should be limited to those who are authorized to have it (privacy of data)
    - **Integrity (I)**: Information is valid and has not been altered in unauthorized ways (authenticity and trustworthiness of information)
    - **Availability (A)**: Information must be accessible to authorized users when they need it; otherwise, the data has no value. Outages not caused by intruders also fall into the category of availability problems.
  
- ==What is social engineering, why is it particularly difficult to defend against, and what is one common form of this attack?==
    - **Social engineering**: The use of psychological influence to persuade people to take actions or reveal information. It exploits human factors rather than software vulnerabilities with goals of information gathering, fraud, or system access.
    - **Why difficult to defend**: Human users (and administrators) are the weakest links in the security chain and no amount of technology can protect against the human element.
    - **Common form**: **Phishing** - attackers deceive people into executing malicious code or revealing sensitive information through deceptive communications (emails, websites, etc.).
  
- ==What is a software vulnerability, what is a specific example of such a vulnerability, and how can open-source code review practices help in reducing these vulnerabilities?== 
    - **Software vulnerability**: A flaw or weakness in a system's design, implementation, or management that can be exploited by an attacker to compromise its security.
    - **Specific example**: **Buffer overflows** - when a program writes more data to a memory buffer than it can hold, potentially allowing attackers to execute arbitrary code.
    - **Open-source code review**: Open-source code leads to better security because the more people who can scrutinize the code, the greater the chance that someone will spot a security weakness ("many eyes make all bugs