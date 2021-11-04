"""Simple tests to check support for the following mempool-related options
- --ignore-node-mempool
- --mempool [file|uri]
"""

import os.path
import json

from http.server import HTTPServer, SimpleHTTPRequestHandler
from multiprocessing import Process

from client.client import Client
from tools import utils


PORT = 8888
MEMPOOL_FILES_DIRECTORY = "mempool_files"
EMPTY_MEMPOOL = "empty_mempool"
ABSENT_MEMPOOL = "this_file_should_not_exist"
SINGLETON_MEMPOOL = "singleton_mempool"
TEST_DIR = "tests_011"


class MyHttpServer:
    """Simple HTTP server launching in a separate process"""

    def __init__(self):
        server_address = ('', PORT)
        httpd = HTTPServer(server_address, SimpleHTTPRequestHandler)
        process = Process(target=httpd.serve_forever, args=())
        self.process = process
        self.server = httpd

    def run(self):
        self.process.start()

    def close(self):
        self.server.server_close()
        self.process.terminate()


def get_filename(basename: str) -> str:
    return os.path.join(TEST_DIR, MEMPOOL_FILES_DIRECTORY, f"{basename}.json")


class TestIgnoreNodeMempool:
    def test_ignore(self, client: Client):
        """Check that a transfer injected into the node is dutifully ignored
        when baking with --ignore-node-mempool
        """
        sender = "bootstrap4"
        balance0 = client.get_balance(sender)
        client.transfer(2, sender, 'bootstrap5')
        utils.bake(
            client, bake_args=['--minimal-timestamp', "--ignore-node-mempool"]
        )
        balance1 = client.get_balance(sender)
        # Make sure the operations has not been included, indirectly through
        # balance checks
        assert balance1 == balance0 

    def test_no_ignore(self, client: Client):
        """Check that a transfer injected, then ignored, can be injected at the
        next block"""
        sender = "bootstrap4"
        balance0 = client.get_balance(sender)
        utils.bake(client, bake_args=['--minimal-timestamp'])
        balance1 = client.get_balance(sender)
        assert balance1 != balance0


class TestNonNodeMempool:
    def test_bake_empty_mempool_file(self, client: Client):
        utils.bake(
            client,
            bake_args=[
                '--minimal-timestamp',
                "--mempool",
                get_filename(EMPTY_MEMPOOL),
            ],
        )

    def test_bake_empty_mempool_http(self, client: Client):
        server = MyHttpServer()
        server.run()
        utils.bake(
            client,
            bake_args=[
                '--minimal-timestamp',
                "--mempool",
                f"http://localhost:{PORT}/{get_filename(EMPTY_MEMPOOL)}",
            ],
        )
        server.close()

    def test_bake_absent_mempool_file(self, client: Client):
        """The absent resource should simply be ignored."""
        utils.bake(
            client,
            bake_args=['--minimal-timestamp', "--mempool", f"{ABSENT_MEMPOOL}"],
        )

    def test_bake_absent_mempool_http(self, client: Client):
        """The absent resource should simply be ignored."""
        server = MyHttpServer()
        server.run()
        utils.bake(
            client,
            bake_args=[
                '--minimal-timestamp',
                "--mempool",
                f"http://{ABSENT_MEMPOOL}",
            ],
        )
        server.close()

    def test_bake_singleton_mempool_file_pre(
        self, client: Client, session: dict
    ):
        """Construct a transaction over the current state, and bake it.
        This serves as a dynamic oracle for the next steps.
        """
        sender = 'bootstrap2'
        balance0 = client.get_mutez_balance(sender)
        session['amount'] = 2
        client.transfer(session['amount'], sender, 'bootstrap3')

        # Baking
        utils.bake(client, bake_args=['--minimal-timestamp'])
        balance1 = client.get_mutez_balance(sender)
        session['difference'] = balance0 - balance1
        utils.bake(client)

    def test_bake_singleton_mempool_file(self, client: Client, session: dict):
        """Construct a transaction over the current state, put it into a file,
        and bake it into the chain through --mempool option.

        This additionally compares the balance to a normal transfer (through the
        node's mempool) to check that there is no observable difference in
        behaviors between passing through a node's mempool or a hand-rolled
        mempool file.
        """
        sender = 'bootstrap4'
        balance0 = client.get_mutez_balance(sender)
        client.transfer(session['amount'], sender, 'bootstrap3')

        pending_ops = client.rpc(
            'get', '/chains/main/mempool/pending_operations'
        )

        # Write the transaction to a file
        file = get_filename(SINGLETON_MEMPOOL)
        with open(file, 'w') as fdesc:
            fdesc.write(json.dumps(pending_ops))

        # Baking
        utils.bake(
            client,
            bake_args=[
                '--minimal-timestamp',
                "--mempool",
                file,
                '--ignore-node-mempool',
            ],
        )
        balance1 = client.get_mutez_balance(sender)
        assert balance0 - balance1 == session['difference']

    def test_bake_singleton_mempool_http(self, client: Client, session: dict):
        # Bake once more to clear pending_operations
        utils.bake(client)

        sender = 'bootstrap2'
        balance0 = client.get_mutez_balance(sender)
        client.transfer(session['amount'], sender, 'bootstrap3')

        pending_ops = client.rpc(
            'get', '/chains/main/mempool/pending_operations'
        )

        assert len(pending_ops['applied'][0]['contents']) == 1

        # Write the transaction to a file
        file = get_filename(SINGLETON_MEMPOOL)
        with open(file, 'w') as fdesc:
            fdesc.write(json.dumps(pending_ops))

        server = MyHttpServer()
        server.run()
        utils.bake(
            client,
            bake_args=[
                '--minimal-timestamp',
                "--mempool",
                f"http://localhost:{PORT}/{file}",
                '--ignore-node-mempool',
            ],
        )
        server.close()
        balance1 = client.get_mutez_balance(sender)
        assert balance0 - balance1 == session['difference']
