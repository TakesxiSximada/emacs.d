#! /usr/bin/env python3
import sys
import http.server
import socketserver

PORT = int(sys.argv[1])


class Handler(http.server.SimpleHTTPRequestHandler):
    def _do_print(self):
        print("===================================================================")
        print(self.requestline)
        for key, val in self.headers.items():
            print(f"{key}: {val}")
        print()
        print(self.rfile.read(int(self.headers["content-length"])).decode("utf8"))
        self.send_response(200, message="OK")
        self.end_headers()
        self.wfile.write("OK".encode())

    def do_GET(self):
        self._do_print()

    def do_POST(self):
        self._do_print()

    def do_DELETE(self):
        self._do_print()


with socketserver.TCPServer(("", PORT), Handler) as httpd:
    print("serving at port", PORT)
    httpd.serve_forever()
