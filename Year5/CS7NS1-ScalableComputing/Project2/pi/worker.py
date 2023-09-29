#!/usr/bin/env python3

import argparse
import asyncio
import aiohttp

PORT = 14285
BATCH_SIZE = 128


class Server:

    def __init__(self, cleaner, decoder):
        self.work = dict()
        self.cleaner = cleaner
        self.decoder = decoder

    def connection_made(self, transport):
        #self.transport.sendto(self.message.encode())
        print("UDP conn:", transport)

    def datagram_received(self, data, addr):
        # data should be a 1 or more urls
        # addr is master to respond to
        print("UDP recv:", addr, data)

    def connection_lost(self, exc):
        print("UDP disc:", exc)

    def error_received(self, exc):
        print("UDP error:", exc)


async def main(cleaner_model_path, decoder_model_path, port):

    # TODO: load tflite models
    cleaner = None
    decoder = None

    # TODO: setup model input/output pipelines

    loop = asyncio.get_running_loop()

    transport, protocol = await loop.create_datagram_endpoint(
        lambda: Server(cleaner, decoder),
        local_addr=("127.0.0.1", port), reuse_port=True)

    try:
        await asyncio.Event().wait()
    finally:
        transport.close()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("cleaner")
    parser.add_argument("decoder")
    parser.add_argument("--port", type=int, default=PORT)
    args = parser.parse_args()
    asyncio.run(main(args.cleaner, args.decoder, args.port))
