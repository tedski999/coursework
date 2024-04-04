from pox.core import core
import pox.lib.packet.ethernet as eth
import pox.lib.packet.ipv4 as ip
import pox.openflow.libopenflow_01 as of
from pox.lib.util import dpidToStr
from pox.lib.addresses import EthAddr

log = core.getLogger()

# Rules table: src mac -> (dst mac -> (dst port -> action))
rules = {
    EthAddr("00:00:00:00:00:01"): {
        EthAddr("00:00:00:00:00:02"): of.ofp_action_output(port=2), # Not capped
        EthAddr("00:00:00:00:00:03"): {
            30: of.ofp_action_enqueue(port=3, queue_id=0), # Capped to 50 Mb/s
        },
        EthAddr("00:00:00:00:00:04"): {
            80: of.ofp_action_enqueue(port=4, queue_id=1), # "Not capped"
        },
    },
    EthAddr("00:00:00:00:00:02"): {
        EthAddr("00:00:00:00:00:01"): of.ofp_action_output(port=1), # Not capped
        EthAddr("00:00:00:00:00:03"): {
            50: of.ofp_action_enqueue(port=3, queue_id=1), # Capped to 100 Mb/s
        },
        EthAddr("00:00:00:00:00:04"): {
            80: of.ofp_action_enqueue(port=4, queue_id=1), # "Not capped"
        },
    },
    EthAddr("00:00:00:00:00:03"): {
        EthAddr("00:00:00:00:00:01"): of.ofp_action_output(port=1), # Not capped
        EthAddr("00:00:00:00:00:02"): of.ofp_action_output(port=2), # Not capped
        EthAddr("00:00:00:00:00:04"): {
            90: of.ofp_action_enqueue(port=4, queue_id=0), # Capped to 500 Mb/s
        },
    },
    EthAddr("00:00:00:00:00:04"): {
        EthAddr("00:00:00:00:00:01"): of.ofp_action_output(port=1), # Not capped
        EthAddr("00:00:00:00:00:02"): of.ofp_action_output(port=2), # Not capped
        EthAddr("00:00:00:00:00:03"): of.ofp_action_enqueue(port=3, queue_id=2), # "Not capped"
    },
}

def launch():
    core.openflow.addListenerByName("ConnectionUp", _handle_ConnectionUp)
    core.openflow.addListenerByName("PacketIn",  _handle_PacketIn)
    log.info("Switch running.")

def _handle_ConnectionUp(event):
    log.info("Starting Switch %s", dpidToStr(event.dpid))
    event.connection.send(of.ofp_flow_mod(command=of.OFPFC_DELETE))

def _handle_PacketIn (event):
    eth_packet = event.parsed
    log.debug("Event: switch %s port %s packet %s" % (dpidToStr(event.dpid), event.port, eth_packet))

    # Deal with ARP packets separately
    if eth_packet.type == eth.ARP_TYPE:
        log.info("Permitting ARP: src=%s dst=%s", eth_packet.src, eth_packet.dst)
        # TODO eth_packet.dst == EthAddr("ff:ff:ff:ff:ff:ff")
        event.connection.send(of.ofp_packet_out(data=event.ofp, action=of.ofp_action_output(port=of.OFPP_ALL)))
        return

    # Get ruleset for source, ignore unknown sources
    src = eth_packet.src
    ruleset = rules.get(src)
    if ruleset is None:
        log.info("Ignoring unknown source: %s" % src)
        return

    # Get rule for destination MAC
    dst = eth_packet.dst
    action = ruleset.get(dst)
    match = of.ofp_match(dl_src=src, dl_dst=dst)

    # Get rule for destination port, ignore unknown packet types
    if type(action) is dict:
        if eth_packet.type != eth.IP_TYPE:
            log.info("Ignoring non-IP type: %s", eth_packet.type)
            return
        ip_packet = eth_packet.payload
        if ip_packet.protocol != ip.TCP_PROTOCOL:
            log.info("Ignoring non-TCP protocol: %s", ip_packet.protocol)
            return
        tcp_packet = ip_packet.payload
        action = action.get(tcp_packet.dstport)
        match.dl_type = 0x800
        match.nw_proto = 6
        match.tp_dst = tcp_packet.dstport

    # Install rule on switch
    if action is not None:
        log.info("Installing rule: src=%s dst=%s port=%s action=%s" % (match.dl_src, match.dl_dst, match.tp_dst, action))
        event.connection.send(of.ofp_flow_mod(match=match, action=action, hard_timeout=50))
        event.connection.send(of.ofp_packet_out(data=event.ofp, action=of.ofp_action_output(port=of.OFPP_TABLE)))

    # Install drop rule on switch
    else:
        log.info("Installing drop rule: src=%s dst=%s port=%s" % (match.dl_src, match.dl_dst, match.tp_dst))
        event.connection.send(of.ofp_flow_mod(match=match, hard_timeout=50))
