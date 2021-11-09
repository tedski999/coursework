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

enum ff_data_type {
	FF_DATA_TYPE_ID = 0,
	FF_DATA_TYPE_ACK,
	FF_DATA_TYPE_MSG,
	FF_DATA_TYPE_SRC,
	FF_DATA_TYPE_DST,
	FF_DATA_TYPE_PORT,
	ff_data_type_len
};

enum ff_ack {
	FF_ACK_OK = 0,
	FF_ACK_PACKET_TOO_LONG,
	FF_ACK_INVALID_PACKET,
	FF_ACK_INVALID_DATA_TYPE,
	FF_ACK_DUPLICATE_DATA_TYPE,
	FF_ACK_UNHANDLED,
	FF_ACK_NO_ID,
	FF_ACK_NO_MSG,
	FF_ACK_NO_SRC,
	FF_ACK_NO_DST,
	FF_ACK_NO_PORT,
	FF_ACK_NO_RECEIVERS,
	FF_ACK_NOT_FOUND,
	FF_ACK_SEND_FAILED,
	ff_ack_len
};

#endif
