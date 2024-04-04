from pox.core import core
import pox.openflow.libopenflow_01 as of
from pox.lib.util import dpidToStr
from pox.lib.addresses import EthAddr

log = core.getLogger()

# Rules table: source mac -> (destination mac -> action)
rules = {
    EthAddr("00:00:00:00:00:01"): {
        EthAddr("00:00:00:00:00:03"): of.ofp_action_enqueue(port=3, queue_id=0), # Capped to 50 Mb/s
        EthAddr("00:00:00:00:00:04"): of.ofp_action_enqueue(port=4, queue_id=1), # "Not capped" (100Gb/s, see note 2)
    },
    EthAddr("00:00:00:00:00:02"): {
        EthAddr("00:00:00:00:00:03"): of.ofp_action_enqueue(port=3, queue_id=1), # Capped to 100 Mb/s
        EthAddr("00:00:00:00:00:04"): of.ofp_action_enqueue(port=4, queue_id=0), # Capped to 500 Mb/s
    },
    EthAddr("00:00:00:00:00:03"): {
        EthAddr("00:00:00:00:00:01"): of.ofp_action_output(port=1), # Not capped
        EthAddr("00:00:00:00:00:02"): of.ofp_action_output(port=2), # Not capped
        EthAddr("00:00:00:00:00:04"): of.ofp_action_enqueue(port=4, queue_id=1), # "Not capped" (100Gb/s, see note 2)
    },
    EthAddr("00:00:00:00:00:04"): {
        EthAddr("00:00:00:00:00:01"): of.ofp_action_output(port=1), # Not capped
        EthAddr("00:00:00:00:00:02"): of.ofp_action_output(port=2), # Not capped
        EthAddr("00:00:00:00:00:03"): of.ofp_action_enqueue(port=3, queue_id=2), # "Not capped" (100Gb/s, see note 2)
    },
}

def launch():
    core.openflow.addListenerByName("ConnectionUp", _handle_ConnectionUp)
    core.openflow.addListenerByName("PacketIn",  _handle_PacketIn)
    log.info("Switch running.")

def _handle_ConnectionUp(event):
    log.info("Starting Switch %s", dpidToStr(event.dpid))
    event.connection.send(of.ofp_flow_mod(command=of.OFPFC_DELETE))

def _handle_PacketIn(event):
    log.debug("Event: switch %s port %s packet %s" % (dpidToStr(event.dpid), event.port, event.parsed))

    # Get ruleset for source, ignore unknown sources
    src = event.parsed.src
    ruleset = rules.get(src)
    if ruleset is None:
        log.info("Ignoring unknown source: %s" % src)
        return

    # Get rule for destination
    dst = event.parsed.dst
    action = ruleset.get(dst)
    match = of.ofp_match(dl_src=src, dl_dst=dst)

    # Install rule on switch
    if action is not None:
        log.info("Installing rule: src %s dst %s action %s" % (match.dl_src, match.dl_dst, action))
        event.connection.send(of.ofp_flow_mod(match=match, action=action, hard_timeout=40))
        event.connection.send(of.ofp_packet_out(data=event.ofp, action=of.ofp_action_output(port=of.OFPP_TABLE)))

    # Install broadcast rule on switch
    elif match.dl_dst == EthAddr("ff:ff:ff:ff:ff:ff"):
        log.info("Installing broadcast rule: src %s actions %s" % (match.dl_src, list(ruleset.values())))
        event.connection.send(of.ofp_flow_mod(match=match, actions=list(ruleset.values()), hard_timeout=40))
        event.connection.send(of.ofp_packet_out(data=event.ofp, action=of.ofp_action_output(port=of.OFPP_TABLE)))

    # Install drop rule on switch (see note 3)
    else:
        log.info("Installing drop rule: src %s dst %s" % (match.dl_src, match.dl_dst))
        event.connection.send(of.ofp_flow_mod(match=match, hard_timeout=40))

# Notes:
#
# 1.
# Rarely, either test h3->h1 or h3->h2 have failed for me with "connect failed: Connection refused".
# Maybe "iperf -s &" is sometimes too slow to start or rules are too slow to install?
#
# 2.
# Without creating another queue for them, h1->h4 and h3->h4 are capped like h2->h4, even if their action is ofp_action_output.
# I guess because there is QoS on the port to h4, the switch will use it regardless of if told to.
# This is fixed by creating a 100Gb/s QoS queue for packets from h1 or h3.
# A similar approach is used to "uncap" communication on the port to h3 from h4.
#
# 3.
# Because ARP requests use broadcast and this solution won't allow broadcasts through the firewall, the "drop" rule to drop similar looking packets without
# going to the controller is never installed during testing. These commands in mininet can be used to test this condition by manually inserting an ARP entry:
# h1 arp --set 10.0.0.2 00:00:00:00:00:02
# h1 ping h2
