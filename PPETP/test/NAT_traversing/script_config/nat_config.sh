#!/bin/sh
#
echo 1 > /proc/sys/net/ipv4/ip_forward
#
/sbin/route add -net 192.168.1.0 netmask 255.255.255.0 dev eth2
/sbin/iptables-restore << EOF
# Generated by iptables-save v1.2.11 on Tue Jan 20 17:26:25 2009
*nat
:PREROUTING ACCEPT [4213:785283]
:POSTROUTING ACCEPT [84:5188]
:OUTPUT ACCEPT [84:5188]
-A POSTROUTING -s 192.168.1.0/255.255.255.0 -j MASQUERADE 
COMMIT
# Completed on Tue Jan 20 17:26:25 2009
# Generated by iptables-save v1.2.11 on Tue Jan 20 17:26:25 2009
*filter
:INPUT ACCEPT [0:0]
:FORWARD ACCEPT [0:0]
:OUTPUT ACCEPT [2017:2277664]
:RH-Firewall-1-INPUT - [0:0]
-A INPUT -j RH-Firewall-1-INPUT 
-A FORWARD -j RH-Firewall-1-INPUT 
-A RH-Firewall-1-INPUT -s 192.168.1.0/255.255.255.0 -j ACCEPT 
-A RH-Firewall-1-INPUT -i lo -j ACCEPT 
-A RH-Firewall-1-INPUT -p icmp -m icmp --icmp-type echo-request -j ACCEPT 
-A RH-Firewall-1-INPUT -p icmp -m icmp --icmp-type echo-reply -j ACCEPT 
-A RH-Firewall-1-INPUT -p udp -m udp --dport 631 -j ACCEPT 
-A RH-Firewall-1-INPUT -m state --state RELATED,ESTABLISHED -j ACCEPT 
-A RH-Firewall-1-INPUT -p tcp -m state --state NEW -m tcp --dport 22 -j ACCEPT 
-A RH-Firewall-1-INPUT -p udp -m state --state NEW -m udp --dport 9005 -j ACCEPT 
-A RH-Firewall-1-INPUT -j REJECT --reject-with icmp-host-prohibited 
COMMIT
# Completed on Tue Jan 20 17:26:25 2009

EOF
