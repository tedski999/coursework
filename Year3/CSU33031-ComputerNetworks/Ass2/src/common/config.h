/**
 * Flow Forwarding
 * Copyright (C) 2021 Ted Johnson TCD 19335618 <edjohnso@tcd.ie>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef FF_CONFIG_H
#define FF_CONFIG_H

#define FF_LOCALHOST "127.0.0.1"
#define FF_PORTNAME "51510"
#define FF_MAX_HOSTNAME_LEN 128
#define FF_MAX_PORTNAME_LEN 128
#define FF_MAX_PACKET_LEN 1024
#define FF_TLV_HEADER_LEN 2
#define FF_REQUEST_TIMEOUT 1000
#define FF_ADDRESS_DELIM '.'
#define FF_ADDRESS_WILDCARD "*"

enum ff_datatype {
	FF_DATATYPE_ACK = 0,  //0
	FF_DATATYPE_PEERADDR, //1
	FF_DATATYPE_SRCADDR,  //2
	FF_DATATYPE_DSTADDR,  //3
	FF_DATATYPE_CLIENT,   //4
	FF_DATATYPE_PAYLOAD,  //5
	ff_datatype_len
};

enum ff_ack {
	FF_ACK_OK = 0,
	FF_ACK_ROUTING,
	FF_ACK_FORWARDING,
	FF_ACK_PACKET_TOO_LONG,
	FF_ACK_INVALID_PACKET,
	FF_ACK_INVALID_DATATYPE,
	FF_ACK_DUPLICATE_DATATYPE,
	FF_ACK_UNHANDLED,
	FF_ACK_NO_RECEIVERS,
	FF_ACK_NOT_FOUND,
	FF_ACK_SEND_FAILED,
	ff_ack_len
};

#endif
