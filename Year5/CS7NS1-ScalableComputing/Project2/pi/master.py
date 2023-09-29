#!/usr/bin/env python3

import argparse
import asyncio
import aiohttp

PORT = 14285
BATCH_SIZE = 128


"""
class Client:

    def __init__(self, addr):
        self.addr = addr

    def connection_made(self, transport):
        #self.transport.sendto(self.message.encode())
        print("UDP conn:", transport)

    def datagram_received(self, data, addr):
        # data should be a 1 or more results
        # addr is worker
        print("UDP recv:", addr, data)

    def connection_lost(self, exc):
        print("UDP disc:", exc)

    def error_received(self, exc):
        print("UDP error:", exc)
"""


async def fetch_filelist(filelist_url, username):
    url = f"{filelist_url}?shortname={username}"
    async with aiohttp.ClientSession() as session:
        async with session.get(url) as response: # TODO(err): request/response error checking and handling
            print(await response.text())


async def process_filelist(filelist_path, worker_addrs):
    with open(args.filelist_path, "r") as f:
        urls = f.readlines() # TODO(err): format error checking and handling
    url_table = { url: [] for url in urls }
    workers = [ _handle_worker(worker_addr, url_table) for worker_addr in worker_addrs ]
    await asyncio.gather(workers)
    for url, entry in url_table:
        print(url, entry) # TODO(err): handle missing


async def _handle_worker(addr, url_table):
    try:
        addr = addr.split(":")
        host, port = addr[0], addr[1:] if len(addr) > 1 else PORT
        #while True:
            # TODO: handle timeout by resending as well as allocating to another
    except:
        # TODO: remove addr from table and reallocate empty entries
        #for url, entry in url_table
        #    remove_self_
        pass


def format_csv(file_path, username):
    with open(file_path) as f:
        print(username)
        for line in sorted(f.readlines()):
            fields = line.split()
            print(f"{fields[0]},{fields[1]}")


if __name__ == "__main__":

    parser = argparse.ArgumentParser()
    parser_subcmd = parser.add_subparsers(dest="subcommand", required=True)

    parser_filelist = parser_subcmd.add_parser("filelist")
    parser_filelist.add_argument("url")
    parser_filelist.add_argument("username")

    parser_process = parser_subcmd.add_parser("process")
    parser_process.add_argument("filelist")
    parser_process.add_argument("workers", nargs="+")

    parser_formatcsv = parser_subcmd.add_parser("formatcsv")
    parser_formatcsv.add_argument("file")
    parser_formatcsv.add_argument("username")

    args = parser.parse_args()

    if args.subcommand == "filelist": asyncio.run(fetch_filelist(args.url, args.username))
    elif args.subcommand == "process": asyncio.run(process_filelist(args.filelist, args.workers))
    elif args.subcommand == "formatcsv": format_csv(args.file, args.username)
