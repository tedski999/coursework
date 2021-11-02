/**
 * Subscriber Publisher Protocol
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

#ifndef SUBPUB_CONFIG_H
#define SUBPUB_CONFIG_H

#define SUBPUB_ACK_MASK 0x80
#define SUBPUB_ID_MASK 0x1f
#define SUBPUB_REQUEST_MASK 0x60
#define SUBPUB_MAX_PACKET_LEN 200

#define SUBPUB_TIMEOUT 0.5
#define SUBPUB_RETRIES 2

#define SUBPUB_IPC_PATH "/tmp/subpub"

#endif
